{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , ScopedTypeVariables
           , DeriveFunctor
           , ExistentialQuantification
           , FlexibleContexts
           , TypeFamilies
           , LambdaCase
           #-}

module Upcast.Resource where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Resource (ResourceT, liftResourceT, runResourceT)
import Control.Monad.Free
import qualified Control.Exception.Lifted as E
import Control.Concurrent (threadDelay)

import Data.Monoid
import Data.Maybe (fromJust)
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson.Types as A
import qualified Data.Vector as V
import Data.Time.Clock
import Data.IORef (newIORef)
import System.FilePath.Posix (splitFileName)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Base64 as Base64

import qualified Network.HTTP.Conduit as HTTP
import qualified Aws
import Aws.Core
import Aws.Query (QueryAPIConfiguration(..))
import Aws.Ec2 (castValue)
import qualified Aws.Ec2 as EC2

import Upcast.State
import Upcast.ATerm (alookupS, alookupSE)
import Upcast.Types
import Upcast.Command

import Upcast.TermSubstitution

type MapCast a = Value -> Map Text a

justCast :: FromJSON a => Value -> a
justCast = fromJust . castValue

mvalues = fmap snd . Map.toList
values = fmap snd . H.toList

forceLookup k v = case alookupSE k v of
                    Left reason -> error $ show reason
                    Right x -> x

mcast :: forall a. FromJSON a => Text -> Value -> [a]
mcast key value = mvalues $ (justCast :: MapCast a) $ forceLookup key value

acast :: forall a. FromJSON a => Text -> Value -> a
acast key value = (justCast :: Value -> a) $ forceLookup key value

scast :: forall a. FromJSON a => Text -> Value -> Maybe a
scast k v = alookupS k v >>= castValue

--
-- orphanarium:
--

instance FromJSON EC2.CreateVpc where
    parseJSON (Object v) = EC2.CreateVpc <$> v .: "cidrBlock" <*> pure EC2.Default
    parseJSON _ = mzero

instance FromJSON EC2.IpProtocol where
    parseJSON (String "tcp") = pure EC2.TCP
    parseJSON (String "udp") = pure EC2.UDP
    parseJSON (String "icmp") = pure EC2.ICMP
    parseJSON (String "all") = pure EC2.All
    parseJSON (Number n) = pure $ EC2.Proto 99
    parseJSON Null = pure EC2.All
    parseJSON _ = mzero

instance FromJSON EC2.IpPermission where
    parseJSON (Object v) = EC2.IpPermission <$> v .: "protocol"
                                            <*> v .:? "fromPort"
                                            <*> v .:? "toPort"
                                            <*> (fmap (\a -> [a]) $ v .: "sourceIp")
    parseJSON _ = mzero

-- | Existential type to contain possible QueryAPI-related transactions for `resourceAWS'
data TX = forall r. (ServiceConfiguration r ~ QueryAPIConfiguration, Transaction r Value) => TX r

-- | Transaction that needs a result, obtained from a Value by Text-typed keypath
data TXR = TXR TX Text

data ResourceF next = AWS TX next
                    | AWSR TXR (Text -> next)
                    | AWSV TX (Value -> next)
                    | Wait TX next
                    deriving (Functor)
type ResourcePlan = Free ResourceF

resourceAWS :: (MonadFree ResourceF m, ServiceConfiguration r ~ QueryAPIConfiguration, Transaction r Value) => r -> m ()
resourceAWS tx = liftF (AWS (TX tx) ())

resourceAWSR :: (MonadFree ResourceF m, ServiceConfiguration r ~ QueryAPIConfiguration, Transaction r Value) => r -> Text -> m Text
resourceAWSR tx k = liftF (AWSR (TXR (TX tx) k) id)

aws :: (MonadFree ResourceF m, ServiceConfiguration r ~ QueryAPIConfiguration, Transaction r Value) => r -> m Value
aws tx = liftF (AWSV (TX tx) id)

wait :: (MonadFree ResourceF m, ServiceConfiguration r ~ QueryAPIConfiguration, Transaction r Value) => r -> m ()
wait tx = liftF (Wait (TX tx) ())

rplan :: (MonadFree ResourceF m, Functor m) => Text -> [EC2.ImportKeyPair] -> Value -> m [(Text, Value)]
rplan expressionName keypairs info = do
    mapM_ (\k -> resourceAWSR k "keyFingerprint") keypairs

    vpcA <- fmap mconcat $ forM vpcs $ \vpc -> do
        vpcId <- resourceAWSR vpc "vpcId"
        return [(EC2.cvpc_cidrBlock vpc, vpcId)]

    (wait . EC2.DescribeVpcs) $ fmap snd vpcA
    resourceAWS (EC2.CreateTags (fmap snd vpcA) defTags)

    -- give the internet to VPCs
    forM (fmap snd vpcA) $ \vpcId -> do
      resourceAWS (EC2.ModifyVpcAttribute vpcId (EC2.EnableDnsSupport True))
      resourceAWS (EC2.ModifyVpcAttribute vpcId (EC2.EnableDnsHostnames True))
      gwId <- resourceAWSR EC2.CreateInternetGateway "internetGatewayId"
      resourceAWS (EC2.AttachInternetGateway gwId vpcId)

      rtbId <- resourceAWSR (EC2.DescribeRouteTables vpcId) "routeTableId"
      resourceAWS (EC2.CreateRoute rtbId "0.0.0.0/0" (EC2.GatewayId gwId))

    subnetA <- fmap mconcat $ forM subnets $ \subnet -> do
        let Just csubnet = flip A.parseMaybe subnet $ \(Object obj) -> do
                                    cidr <- obj .: "cidrBlock"
                                    vpc <- obj .: "vpc"
                                    zone <- obj .: "zone"
                                    let Just vpcId = lookup vpc vpcA
                                    return $ EC2.CreateSubnet vpcId cidr $ Just zone
        subnetId <- resourceAWSR csubnet "subnetId"

        return [(EC2.csub_cidrBlock csubnet, subnetId)]

    (wait . EC2.DescribeSubnets) $ fmap snd subnetA
    resourceAWS (EC2.CreateTags (fmap snd subnetA) defTags)

    sgA <- fmap mconcat $ forM secGroups $ \sg -> do
        -- AWS needs more than one transaction for each security group (Create + add rules)
        let Just g = flip A.parseMaybe sg $ \(Object obj) -> do
                                    name <- obj .: "name" :: A.Parser Text
                                    desc <- obj .: "description" :: A.Parser Text
                                    vpc <- obj .: "vpc"
                                    let Just vpcId = lookup vpc vpcA
                                    return $ EC2.CreateSecurityGroup name desc $ Just vpcId
        secGroupId <- resourceAWSR g "groupId"

        let Just r = flip A.parseMaybe sg $ \(Object obj) -> do
                                    rules <- obj .: "rules" :: A.Parser [EC2.IpPermission]
                                    return $ EC2.AuthorizeSecurityGroupIngress secGroupId rules
        resourceAWS r

        return [(EC2.csec_name g, secGroupId)]

    (wait . flip EC2.DescribeSecurityGroups []) $ fmap snd sgA
    resourceAWS (EC2.CreateTags (fmap snd sgA) defTags)

    volumeA <- fmap mconcat $ forM volumes $ \vol -> do
        let Just (name, v) = flip A.parseMaybe vol $ \(Object obj) -> do
                                    name <- obj .: "name" :: A.Parser Text

                                    cvol_AvailabilityZone <- obj .: "zone"
                                    ebd_snapshotId <- obj .: "snapshot" >>= \case "" -> return Nothing; x -> return $ Just x
                                    let ebd_deleteOnTermination = False
                                    let ebd_volumeType = EC2.Standard
                                    ebd_volumeSize <- obj .: "size"
                                    let ebd_encrypted = False

                                    let cvol_ebs = EC2.EbsBlockDevice{..}

                                    return $ (name, EC2.CreateVolume{..})

        volumeId <- resourceAWSR v "volumeId"

        (wait . EC2.DescribeVolumes) [volumeId]
        resourceAWS (EC2.CreateTags [volumeId] (("Name", name):defTags))

        return [(name, volumeId)]

    instanceA <- fmap mconcat $ forM instances $ \inst -> do
        let Just (name, blockDevs, cinst) = flip A.parseMaybe inst $ \(Object obj) -> do
                                    String name <- obj .: "targetHost"
                                    Object ec2 <- obj .: "ec2" :: A.Parser Value
                                    securityGroupNames <- ec2 .: "securityGroups"
                                    subnet <- ec2 .: "subnet"
                                    let Just subnetId = lookup subnet subnetA
                                    let blockDevs = scast "blockDeviceMapping" (Object ec2) :: Maybe (Map Text Value)

                                    run_imageId <- ec2 .: "ami"
                                    let run_count = (1, 1)
                                    run_instanceType <- ec2 .: "instanceType"
                                    let run_securityGroupIds = fmap (fromJust . flip lookup sgA) securityGroupNames
                                    -- run_blockDeviceMappings :: [BlockDeviceMapping]
                                    run_blockDeviceMappings <- return []
                                    let Just run_subnetId = lookup subnet subnetA
                                    let run_monitoringEnabled = True
                                    let run_disableApiTermination = False
                                    let run_instanceInitiatedShutdownBehavior = EC2.Stop
                                    run_ebsOptimized <- ec2 .: "ebsOptimized"
                                    run_keyName <- ec2 .:? "keyPair"
                                    let run_userData = Nothing
                                    let run_kernelId = Nothing
                                    let run_ramdiskId = Nothing
                                    let run_clientToken = Just name -- need a unique string to prevent substituting one call for many
                                    run_availabilityZone <- ec2 .:? "zone"
                                   
                                    return (name, maybe [] Map.toList blockDevs, EC2.RunInstances{..})
        instanceId <- resourceAWSR cinst "instancesSet.instanceId"
        resourceAWS (EC2.CreateTags [instanceId] (("Name", name):defTags))
        return [(name, (instanceId, blockDevs))]

    let instanceIds = fmap (fst . snd) instanceA

    (wait . EC2.DescribeInstanceStatus) instanceIds

    forM (fmap snd instanceA) $ \(avol_instanceId, blockDevs) -> do
      let blockA = (\(mapping, v) -> (mapping, let t = acast "disk" v :: Text
                                                   td = T.drop 4 t
                                                   in case "res-" `T.isPrefixOf` t of
                                                        True -> case lookup td volumeA of
                                                                  Nothing -> error $ mconcat [show td, " not found in volumeA = ", show volumeA]
                                                                  Just x -> x
                                                        False -> error $ mconcat ["can not handle disk: ", T.unpack t])) <$> blockDevs
      forM_ blockA $ \(avol_device, avol_volumeId) -> do
        resourceAWS EC2.AttachVolume{..}

    Array instanceInfos <- aws (EC2.DescribeInstances instanceIds)
    return $ zip (fmap fst instanceA) (V.toList instanceInfos)
  where
    cast :: FromJSON a => Text -> [a]
    cast = (`mcast` info)

    defTags = [("created-using", "upcast"), ("expression", expressionName)]

    vpcs = cast "resources.vpc" :: [EC2.CreateVpc]
    subnets = cast "resources.subnets" :: [Value]
    secGroups = cast "resources.ec2SecurityGroups" :: [Value]
    volumes = cast "resources.ebsVolumes" :: [Value]
    instances = cast "machines" :: [Value]


canonicalSigData :: IO SignatureData
canonicalSigData = do
    emptyRef <- newIORef []
    return SignatureData { signatureTimeInfo = AbsoluteTimestamp baseTime
                         , signatureTime = baseTime
                         , signatureCredentials = Credentials "" "" emptyRef
                         }
  where
    baseTime = UTCTime (toEnum 0) $ secondsToDiffTime 0

data EvalContext = EvalContext
               { mgr :: HTTP.Manager
               , qapi :: QueryAPIConfiguration NormalQuery
               }

substituteTX :: SubStore -> TX -> EvalContext -> ResourceT IO (Sub, SubStore, Value)
substituteTX state (TX tx) EvalContext{..} = do
    awsConf <- liftIO $ Aws.baseConfiguration
    sig <- liftIO $ canonicalSigData
    let s = signQuery tx qapi sig
    let Just (HTTP.RequestBodyBS key) = sqBody s
    liftIO $ BS.putStrLn key
    liftIO $ substitute state (T.decodeUtf8 key) (runResourceT $ Aws.pureAws awsConf qapi mgr tx)

evalPlan :: SubStore -> ResourcePlan a -> ReaderT EvalContext (ResourceT IO) a
evalPlan state (Free (AWSR (TXR tx keyPath) next)) = do
    sub@(t, state', val) <- ask >>= liftResourceT . substituteTX state tx
    let result = acast keyPath val :: Text
    -- liftIO $ print (t, val, result)
    evalPlan state' $ next result
evalPlan state (Free (AWS tx next)) = do
    sub@(t, state', val) <- ask >>= liftResourceT . substituteTX state tx
    -- liftIO $ print (t, val)
    evalPlan state' next
evalPlan state (Free (AWSV (TX tx) next)) = do
    EvalContext{..} <- ask
    awsConf <- liftIO $ Aws.baseConfiguration
    result <- liftResourceT $ Aws.pureAws awsConf qapi mgr tx
    evalPlan state $ next result
evalPlan state (Free (Wait (TX tx) next)) = do
    EvalContext{..} <- ask
    awsConf <- liftIO $ Aws.baseConfiguration
    r <- liftIO $ runResourceT $ retryAws awsConf qapi mgr tx
    -- liftIO $ print r
    evalPlan state next
evalPlan state (Pure r) = return r


retryAws :: (ServiceConfiguration r ~ QueryAPIConfiguration, Transaction r Value) =>
            Aws.Configuration -> QueryAPIConfiguration NormalQuery -> HTTP.Manager -> r -> ResourceT IO Value
retryAws awsConf conf mgr tx = loop 
  where
    loop = do
      result <- catchAll $ Aws.pureAws awsConf conf mgr tx
      case result of
        Left x -> warn x >> loop
        Right Null -> warn Null >> loop
        Right y -> return y

    warn val = liftIO $ BS.putStrLn (mconcat ["wait: retrying after 1: ", BS.pack $ show val]) >> threadDelay 1000000

    catchAll :: ResourceT IO Value -> ResourceT IO (Either E.SomeException Value)
    catchAll = E.handle (return . Left) . fmap Right


data Machine = Machine
             { m_hostname :: Text
             , m_publicIp :: Text
             , m_privateIp :: Text
             , m_instanceId :: Text
             , m_keyFile :: Text
             } deriving (Show)

findRegions :: [Text] -> Value -> [Text]
findRegions acc (Object h) = mappend nacc $ join (findRegions [] <$> (fmap snd $ H.toList h))
  where
    nacc = maybe [] (\case String x -> [x]; _ -> []) $ H.lookup "region" h
findRegions acc (Array v) = mappend acc $ join (findRegions [] <$> V.toList v)
findRegions acc _ = acc

evalResources :: DeployContext -> Value -> IO [(Text, Machine)]
evalResources ctx@DeployContext{..} info = do
    region <- let regions = L.nub $ findRegions [] info
                  in case regions of
                       reg:[] -> return reg
                       _ -> error $ mconcat [ "can only operate with expressions that do not span multiple EC2 regions, given: "
                                            , show regions
                                            ]
    store <- loadSubStore stateFile

    -- pre-calculate EC2.ImportKeyPair values here because we need to do IO
    keypairs <- fmap mconcat $ forM (mcast "resources.ec2KeyPairs" info :: [Value]) $ \keypair -> do
                    let Just (kName, kPK) = flip A.parseMaybe keypair $ \(Object obj) -> do
                                                  kName <- obj .: "name" :: A.Parser Text
                                                  kPK <- obj .: "privateKeyFile" :: A.Parser Text
                                                  return $ (kName, kPK)
                    pubkey <- fgconsume $ Cmd Local $ mconcat ["ssh-keygen -f ", T.unpack kPK, " -y"]
                    return [(kPK, EC2.ImportKeyPair kName $ T.decodeUtf8 $ Base64.encode pubkey)]

    let keypair = fst $ head keypairs

    instances <- HTTP.withManager $ \mgr -> do
      evalPlan store (rplan name (snd <$> keypairs) info) `runReaderT` EvalContext mgr (QueryAPIConfiguration $ T.encodeUtf8 region)

    -- mapM_ LBS.putStrLn $ fmap A.encodePretty instances

    return $ fmap (toMachine keypair) instances
  where
    name = T.pack $ snd $ splitFileName $ T.unpack expressionFile
    
    toMachine k (h, info) = (h, Machine h -- (cast "instancesSet.dnsName" :: Text)
                                        (cast "instancesSet.ipAddress" :: Text)
                                        (cast "instancesSet.privateIpAddress" :: Text)
                                        (cast "instancesSet.instanceId" :: Text)
                                        k)
      where
        cast :: FromJSON a => Text -> a
        cast = (`acast` info)
