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

import qualified Data.ByteString.Lazy as LBS

import Aws.Ec2.Types (castValue)
import qualified Aws.Ec2.Info as EC2
import qualified Aws.Ec2.Commands.CreateVpc as EC2
import qualified Aws.Ec2.Commands.CreateSubnet as EC2
import qualified Aws.Ec2.Commands.ImportKeyPair as EC2
import qualified Aws.Ec2.Commands.CreateVolume as EC2
import qualified Aws.Ec2.Commands.RunInstances as EC2
-- import qualified Aws.Ec2.Commands.CreateSecurityGroup as EC2

import Upcast.State
import Upcast.ATerm (alookupS)
import Upcast.DeployCommands
import Upcast.Deploy

import Upcast.TermSubstitution

type MapCast a = Value -> Map Text a

justCast :: FromJSON a => Value -> a
justCast = fromJust . castValue

mvalues = fmap snd . Map.toList
values = fmap snd . H.toList

mcast :: forall a. FromJSON a => Text -> Value -> [a]
mcast key value = mvalues $ (justCast :: MapCast a) $ fromJust $ alookupS key value


instance FromJSON EC2.CreateVpc where
    parseJSON (Object v) = EC2.CreateVpc <$> v .: "cidrBlock" <*> pure EC2.Default
    parseJSON _ = mzero

instance FromJSON EC2.CreateSubnet where
    parseJSON (Object v) = EC2.CreateSubnet <$> v .: "vpc" {- still not an id XXX -}
                                            <*> v .: "cidrBlock"
    parseJSON _ = mzero

data ResourceF next = VPC EC2.CreateVpc (Text -> next)
                    | Subnet Text EC2.CreateSubnet (EC2.CreateSubnet -> next)
                    | Val Value next
                    deriving (Functor)

type ResourcePlan = Free ResourceF

resourceVpc x = liftF (VPC x id)
resourceSubnet x y = liftF (Subnet x y id)
resourceVal x = liftF (Val x ())

rplan :: MonadFree ResourceF m => Value -> m ()
rplan info = do
    vpcId <- resourceVpc vpc
    subnet <- resourceSubnet vpcId subnet

    return ()
  where
    cast :: FromJSON a => Text -> [a]
    cast = (`mcast` info)

    vpcs@(vpc:_) = cast "resources.vpc" :: [EC2.CreateVpc]
    subnets@(subnet:_) = cast "resources.subnets" :: [EC2.CreateSubnet]
    -- secgroups@(secgroup:_) = mvalues $ (justCast :: MapCast EC2.CreateSecurityGroup) $ fromJust $ alookupS "resources.ec2SecurityGroups" info
    -- keypairs@(keypair:_) = cast "resources.ec2KeyPairs" :: [EC2.ImportKeyPair]
    -- volumes@(vol:_) = cast "resources.ebsVolumes" :: [EC2.CreateVolume]
    -- instances@(inst:_) = cast "resources.machines" :: [EC2.RunInstances]


debug :: SubStore -> ResourcePlan a -> IO a
debug state (Free (VPC value next)) = do
    sub@(_, state', String val) <- substitute state value (return $ String "vpc-00blahg")
    print sub
    debug state' $ next val
debug state (Free (Subnet vpcid create next)) = do
    let v = create{ EC2.csub_vpcId = vpcid }
    sub@(_, state', res) <- substitute state v (return $ String "subnet-11meh")
    print sub
    debug state $ next v
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



