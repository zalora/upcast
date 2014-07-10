{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , FlexibleContexts
           , ScopedTypeVariables
           , DeriveFunctor
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

data ResourceF next = VPC EC2.CreateVpc (Text -> next)
                    | Subnet EC2.CreateSubnet (Text -> next)
                    | SecurityGroup EC2.CreateSecurityGroup (Text -> next)
                    | SecurityGroupAuth EC2.AuthorizeSecurityGroupIngress next
                    | KeyPair EC2.ImportKeyPair next
                    | Val Value next
                    deriving (Functor)

type ResourcePlan = Free ResourceF

resourceVpc x = liftF (VPC x id)
resourceSubnet x = liftF (Subnet x id)
resourceVal x = liftF (Val x ())
resourceSecurityGroup x = liftF (SecurityGroup x id)
resourceSecurityGroupAuth x = liftF (SecurityGroupAuth x id)
resourceKeyPair x = liftF (KeyPair x ())

rplan :: MonadFree ResourceF m => Value -> m ()
rplan info = do
    vpcId <- resourceVpc vpc
    subnet <- resourceSubnet subnet{ EC2.csub_vpcId = vpcId }

    -- AWS needs more than one call per each security group (Create + add rules)
    let Just g = flip A.parseMaybe sg $ \(Object obj) -> do
                                name <- obj .: "name" :: A.Parser Text
                                desc <- obj .: "description" :: A.Parser Text
                                return $ EC2.CreateSecurityGroup name desc $ Just vpcId
    secGroupId <- resourceSecurityGroup g

    let Just r = flip A.parseMaybe sg $ \(Object obj) -> do
                                rules <- obj .: "rules" :: A.Parser [EC2.IpPermission]
                                return $ EC2.AuthorizeSecurityGroupIngress secGroupId rules
    resourceSecurityGroupAuth r

    resourceKeyPair keypair

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


debug :: SubStore -> ResourcePlan a -> IO a
debug state (Free (VPC value next)) = do
    sub@(_, state', String val) <- substitute state value (return $ String "vpc-00blahg")
    print sub
    debug state' $ next val
debug state (Free (Subnet create next)) = do
    sub@(_, state', String res) <- substitute state create (return $ String "subnet-11meh")
    print sub
    debug state' $ next res
debug state (Free (SecurityGroup create next)) = do
    sub@(_, state', String res) <- substitute state create (return $ String "sg-22wow")
    print sub
    debug state' $ next res
debug state (Free (SecurityGroupAuth value next)) = do
    -- TODO ignore InvalidPermission.Duplicate
    print value
    debug state next
debug state (Free (KeyPair value next)) = do
    -- TODO ignore InvalidKeyPair.Duplicate
    print value
    debug state next
debug state (Free (Val value next)) = do
    print value
    debug state next
debug state (Pure r) = return r


evalResources exprFile ctx@DeployContext{..} = do
    let s = emptyState exprFile
    Right info <- deploymentInfo ctx s
    store <- loadSubStore "store"
    
    debug store $ rplan info
    return ()



