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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Time.Clock
import Data.IORef (newIORef)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base64 as Base64

import qualified Network.HTTP.Conduit as HTTP
import qualified Aws
import Aws.Core
import Aws.Ec2 (castValue, EC2Configuration(..))
import qualified Aws.Ec2 as EC2

import Upcast.State
import Upcast.ATerm (alookupS, alookupSE)
import Upcast.DeployCommands
import Upcast.Deploy
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

-- | Existential type to contain possible EC2-related transactions for `resourceAWS'
data TX = forall r. (ServiceConfiguration r ~ EC2Configuration, Transaction r Value) => TX r

-- | Transaction that needs a result, obtained from a Value by Text-typed keypath
data TXR = TXR TX Text

data ResourceF next = AWS TX next
                    | AWSR TXR (Text -> next)
                    | Wait TX next
                    deriving (Functor)
type ResourcePlan = Free ResourceF

resourceAWS tx = liftF (AWS tx ())
resourceAWSR txr = liftF (AWSR txr id)
wait tx = liftF (Wait tx ())

rplan :: (MonadFree ResourceF m, Functor m) => [EC2.ImportKeyPair] -> Value -> m ()
rplan keypairs info = do
    mapM_ (\k -> resourceAWSR $ TXR (TX k) "keyFingerprint") keypairs

    vpcA <- fmap mconcat $ forM vpcs $ \vpc -> do
        vpcId <- resourceAWSR $ TXR (TX vpc) "vpcId"
        return [(EC2.cvpc_cidrBlock vpc, vpcId)]

    (wait . TX . EC2.DescribeVpcs) $ fmap snd vpcA

    -- give the internet to VPCs
    forM (fmap snd vpcA) $ \vpcId -> do
      resourceAWS $ TX (EC2.ModifyVpcAttribute vpcId (EC2.EnableDnsSupport True))
      resourceAWS $ TX (EC2.ModifyVpcAttribute vpcId (EC2.EnableDnsHostnames True))
      gwId <- resourceAWSR $ TXR (TX EC2.CreateInternetGateway) "internetGatewayId"
      resourceAWS $ TX (EC2.AttachInternetGateway gwId vpcId)

      rtbId <- resourceAWSR $ TXR (TX (EC2.DescribeRouteTables vpcId)) "routeTableId"
      resourceAWS $ TX (EC2.CreateRoute rtbId "0.0.0.0/0" (EC2.GatewayId gwId))

    subnetA <- fmap mconcat $ forM subnets $ \subnet -> do
        let Just csubnet = flip A.parseMaybe subnet $ \(Object obj) -> do
                                    cidr <- obj .: "cidrBlock"
                                    vpc <- obj .: "vpc"
                                    let Just vpcId = lookup vpc vpcA
                                    return $ EC2.CreateSubnet vpcId cidr
        subnetId <- resourceAWSR $ TXR (TX csubnet) "subnetId"

        return [(EC2.csub_cidrBlock csubnet, subnetId)]

    (wait . TX . EC2.DescribeSubnets) $ fmap snd subnetA

    sgA <- fmap mconcat $ forM secGroups $ \sg -> do
        -- AWS needs more than one transaction for each security group (Create + add rules)
        let Just g = flip A.parseMaybe sg $ \(Object obj) -> do
                                    name <- obj .: "name" :: A.Parser Text
                                    desc <- obj .: "description" :: A.Parser Text
                                    vpc <- obj .: "vpc"
                                    let Just vpcId = lookup vpc vpcA
                                    return $ EC2.CreateSecurityGroup name desc $ Just vpcId
        secGroupId <- resourceAWSR $ TXR (TX g) "groupId"

        let Just r = flip A.parseMaybe sg $ \(Object obj) -> do
                                    rules <- obj .: "rules" :: A.Parser [EC2.IpPermission]
                                    return $ EC2.AuthorizeSecurityGroupIngress secGroupId rules
        resourceAWS $ TX r

        return [(EC2.csec_name g, secGroupId)]

    (wait . TX . flip EC2.DescribeSecurityGroups []) $ fmap snd sgA

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

        volumeId <- resourceAWSR $ TXR (TX v) "volumeId"
        return [(name, volumeId)]

    (wait . TX . EC2.DescribeVolumes) $ fmap snd volumeA

    instanceA <- fmap mconcat $ forM instances $ \inst -> do
        let Just (name, cinst) = flip A.parseMaybe inst $ \(Object obj) -> do
                                    String name <- obj .: "targetHost"
                                    Object ec2 <- obj .: "ec2" :: A.Parser Value
                                    securityGroupNames <- ec2 .: "securityGroups"
                                    subnet <- ec2 .: "subnet"
                                    let Just subnetId = lookup subnet subnetA
                                    -- TODO:
                                    -- let bds = acast "blockDeviceMapping" (Object ec2) :: Map Text Value
                                    -- Map.foldMapWithKey bds `Map.foldMapWithKey` $ \k v ->
                                    --                EC2.BlockDeviceMapping k $ EC2.EBS EC2.EbsBlockDevice{..}

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
                                    return (name, EC2.RunInstances{..})
        instanceId <- resourceAWSR $ TXR (TX cinst) "instancesSet.instanceId"
        return [(name, instanceId)]

    (wait . TX . EC2.DescribeInstanceStatus) $ fmap snd instanceA

    -- TODO: attach volumes

    return ()
  where
    cast :: FromJSON a => Text -> [a]
    cast = (`mcast` info)

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

substituteTX :: SubStore -> TX -> HTTP.Manager -> ResourceT IO (Sub, SubStore, Value)
substituteTX state (TX tx) mgr = do
    awsConf <- liftIO $ Aws.dbgConfiguration
    sig <- liftIO $ canonicalSigData
    let s = signQuery tx conf sig
    let Just (HTTP.RequestBodyBS key) = sqBody s
    liftIO $ BS.putStrLn key
    liftIO $ substitute state (T.decodeUtf8 key) (runResourceT $ Aws.pureAws awsConf conf mgr tx)
  where
    conf = EC2Configuration "eu-west-1"


evalPlan :: SubStore -> ResourcePlan a -> ReaderT HTTP.Manager (ResourceT IO) a
evalPlan state (Free (AWSR (TXR tx keyPath) next)) = do
    sub@(t, state', val) <- ask >>= liftResourceT . substituteTX state tx
    let result = acast keyPath val :: Text
    -- let result = acast "blahId" val :: Text
    liftIO $ print (t, val, result)
    evalPlan state' $ next result
evalPlan state (Free (AWS tx next)) = do
    sub@(t, state', val) <- ask >>= liftResourceT . substituteTX state tx
    liftIO $ print (t, val)
    evalPlan state' next
evalPlan state (Free (Wait (TX tx) next)) = do
    awsConf <- liftIO $ Aws.baseConfiguration
    mgr <- ask
    let conf = EC2Configuration "eu-west-1"
    r <- liftIO $ runResourceT $ retryAws awsConf conf mgr tx
    liftIO $ print r
    evalPlan state next
evalPlan state (Pure r) = return r


retryAws :: (ServiceConfiguration r ~ EC2Configuration, Transaction r Value) =>
            Aws.Configuration -> EC2Configuration NormalQuery -> HTTP.Manager -> r -> ResourceT IO Value
retryAws awsConf conf mgr tx = loop 
  where
    loop = do
      result <- catchAll $ Aws.pureAws awsConf conf mgr tx
      case result of
        Left x -> warn x >> loop
        Right Null -> warn Null >> loop
        Right y -> return y

    warn val = liftIO $ BS.putStrLn (mconcat ["retrying after 1: ", BS.pack $ show val]) >> threadDelay 1000000

    catchAll :: ResourceT IO Value -> ResourceT IO (Either E.SomeException Value)
    catchAll = E.handle (return . Left) . fmap Right


evalResources exprFile ctx@DeployContext{..} = do
    let s = emptyState exprFile
    Right info <- deploymentInfo ctx s
    store <- loadSubStore "store"

    -- pre-calculate EC2.ImportKeyPair values here because we need to do IO
    keypairs <- fmap mconcat $ forM (mcast "resources.ec2KeyPairs" info :: [Value]) $ \keypair -> do
                    let Just (kName, kPK) = flip A.parseMaybe keypair $ \(Object obj) -> do
                                                  kName <- obj .: "name" :: A.Parser Text
                                                  kPK <- obj .: "privateKeyFile" :: A.Parser Text
                                                  return $ (kName, kPK)
                    pubkey <- fgconsume $ Cmd Local $ mconcat ["ssh-keygen -f ", T.unpack kPK, " -y"]
                    return [EC2.ImportKeyPair kName $ T.decodeUtf8 $ Base64.encode pubkey]

    HTTP.withManager $ runReaderT $ evalPlan store (rplan keypairs info)
    return ()

