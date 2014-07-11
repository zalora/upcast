{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , ScopedTypeVariables
           , DeriveFunctor
           , ExistentialQuantification
           , FlexibleContexts
           , TypeFamilies
           #-}

module Upcast.Resource where

import Control.Applicative
import Control.Monad
import Control.Monad.Free

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

import qualified Data.ByteString.Lazy as LBS

import Aws.Ec2.Types (castValue)
import qualified Aws.Ec2.Info as EC2
import qualified Aws.Ec2.Commands.CreateVpc as EC2
import qualified Aws.Ec2.Commands.CreateSubnet as EC2
import qualified Aws.Ec2.Commands.ImportKeyPair as EC2
import qualified Aws.Ec2.Commands.CreateVolume as EC2
import qualified Aws.Ec2.Commands.RunInstances as EC2
import qualified Aws.Ec2.Commands.CreateSecurityGroup as EC2
import qualified Aws.Ec2.Commands.AuthorizeSecurityGroupIngress as EC2

import Upcast.State
import Upcast.ATerm (alookupS, alookupSE)
import Upcast.DeployCommands
import Upcast.Deploy

import Upcast.TermSubstitution

import Aws.Core
import Data.Time.Clock
import Data.IORef (newIORef)
import Aws.Ec2.Core (EC2Configuration(..))
import qualified Network.HTTP.Conduit as HTTP

type MapCast a = Value -> Map Text a

justCast :: FromJSON a => Value -> a
justCast = fromJust . castValue

fromRight (Right r) = r

mvalues = fmap snd . Map.toList
values = fmap snd . H.toList

mcast :: forall a. FromJSON a => Text -> Value -> [a]
mcast key value = mvalues $ (justCast :: MapCast a) $ fromRight $ alookupSE key value

acast :: forall a. FromJSON a => Text -> Value -> a
acast key value = (justCast :: Value -> a) $ fromRight $ alookupSE key value

--
-- orphanarium:
--

instance FromJSON EC2.CreateVpc where
    parseJSON (Object v) = EC2.CreateVpc <$> v .: "cidrBlock" <*> pure EC2.Default
    parseJSON _ = mzero

instance FromJSON EC2.CreateSubnet where
    parseJSON (Object v) = EC2.CreateSubnet <$> pure undefined {- vpc placeholder -}
                                            <*> v .: "cidrBlock"
    parseJSON _ = mzero

instance FromJSON EC2.CreateSecurityGroup where
    parseJSON (Object v) = EC2.CreateSecurityGroup <$> v .: "name"
                                                   <*> v .: "description"
                                                   <*> pure undefined {- vpc placeholder -}
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

instance FromJSON EC2.ImportKeyPair where
    parseJSON (Object v) = EC2.ImportKeyPair <$> v .: "name"
                                             <*> v .: "privateKeyFile" {- XXX: not encoded! -}

-- | Existential type to contain possible EC2-related transactions for `resourceAWS'
data TX = forall r. (ServiceConfiguration r ~ EC2Configuration, Transaction r Value) => TX r

-- | Transaction that needs a result, obtained from a Value by Text-typed keypath
data TXR = TXR TX Text

data ResourceF next = AWS TX next
                    | AWSR TXR (Text -> next)
                    deriving (Functor)
type ResourcePlan = Free ResourceF

resourceAWS tx = liftF (AWS tx ())
resourceAWSR txr = liftF (AWSR txr id)

rplan :: MonadFree ResourceF m => Value -> m ()
rplan info = do
    vpcId <- resourceAWSR $ TXR (TX vpc) "vpcId"
    subnet <- resourceAWSR $ TXR (TX subnet{ EC2.csub_vpcId = vpcId }) "subnetId"

    -- AWS needs more than one call per each security group (Create + add rules)
    let Just g = flip A.parseMaybe sg $ \(Object obj) -> do
                                name <- obj .: "name" :: A.Parser Text
                                desc <- obj .: "description" :: A.Parser Text
                                return $ EC2.CreateSecurityGroup name desc $ Just vpcId
    secGroupId <- resourceAWSR $ TXR (TX g) "groupId"

    let Just r = flip A.parseMaybe sg $ \(Object obj) -> do
                                rules <- obj .: "rules" :: A.Parser [EC2.IpPermission]
                                return $ EC2.AuthorizeSecurityGroupIngress secGroupId rules
    resourceAWS $ TX r

    resourceAWSR $ TXR (TX keypair) "keyFingerprint"

    return ()
  where
    cast :: FromJSON a => Text -> [a]
    cast = (`mcast` info)

    acast' :: FromJSON a => Text -> a
    acast' = (`acast` info)

    vpcs@(vpc:_) = cast "resources.vpc" :: [EC2.CreateVpc]
    subnets@(subnet:_) = cast "resources.subnets" :: [EC2.CreateSubnet]

    secGroups@(sg:_) = cast "resources.ec2SecurityGroups" :: [Value]
    keypairs@(keypair:_) = cast "resources.ec2KeyPairs" :: [EC2.ImportKeyPair]

    -- volumes@(vol:_) = cast "resources.ebsVolumes" :: [EC2.CreateVolume]
    -- instances@(inst:_) = cast "resources.machines" :: [EC2.RunInstances]



canonicalSigData :: IO SignatureData
canonicalSigData = do
    emptyRef <- newIORef []
    return SignatureData { signatureTimeInfo = AbsoluteTimestamp baseTime
                         , signatureTime = baseTime
                         , signatureCredentials = Credentials "" "" emptyRef
                         }
  where
    baseTime = UTCTime (toEnum 0) $ secondsToDiffTime 0

substituteTX :: SubStore -> TX -> IO (Sub, SubStore, Value)
substituteTX state (TX tx) =  do
    -- TODO ignore InvalidPermission.Duplicate
    --      ignore InvalidKeyPair.Duplicate
    sig <- canonicalSigData
    let s = signQuery tx conf sig
    let Just (HTTP.RequestBodyBS key) = sqBody s
    substitute state (T.decodeUtf8 key) (return $ object $ [("blahId", String "tx-4321")])
  where
    conf = EC2Configuration "us-east-1"


debug :: SubStore -> ResourcePlan a -> IO a
debug state (Free (AWSR (TXR tx keyPath) next)) = do
    sub@(t, state', val) <- substituteTX state tx
    -- let result = acast keyPath val :: Text
    let result = acast "blahId" val :: Text
    print (t, val, result)
    debug state' $ next result
debug state (Free (AWS tx next)) = do
    sub@(t, state', val) <- substituteTX state tx
    print (t, val)
    debug state' $ next
debug state (Pure r) = return r


evalResources exprFile ctx@DeployContext{..} = do
    let s = emptyState exprFile
    Right info <- deploymentInfo ctx s
    store <- loadSubStore "store"
    
    debug store $ rplan info
    return ()


