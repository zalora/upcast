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
import Data.Map (Map)
import qualified Data.Map as Map
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

import Upcast.Interpolate (n)
import Upcast.State
import Upcast.Nix
import Upcast.Command
import Upcast.Temp
import Upcast.ATerm (alookupS)
import Upcast.DeployCommands
import Upcast.Deploy

type MapCast a = Value -> Map Text a
justCast :: FromJSON a => Value -> a
justCast = fromJust . castValue

mvalues = fmap snd . Map.toList
values = fmap snd . H.toList

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
    let vpcs@(vpc:_) = mvalues $ (justCast :: MapCast EC2.CreateVpc) $ fromJust $ alookupS "resources.vpc" info
    let subnets@(subnet:_) = mvalues $ (justCast :: MapCast EC2.CreateSubnet) $ fromJust $ alookupS "resources.subnets" info

    vpcId <- resourceVpc vpc
    subnet <- resourceSubnet vpcId subnet

    return ()

data S = Cached | Created
       deriving (Show)

data V = V S Text TransactionStore Text
       deriving (Show)

debug :: TransactionStore -> ResourcePlan a -> IO a
debug state (Free (VPC value next)) = do
    let key :: Text = T.pack $ show value
    v@(V _ _ state'' vpcid) <- case clookup key state of
                             Nothing -> do
                               let result = "vpc-00blah" :: Text
                               state' <- commit (cache key (String result) state) "store"
                               return $ V Created key state' result
                             Just x -> return $ V Cached key state $ (justCast :: Value -> Text) x
    print v
    debug state'' $ next vpcid
debug state (Free (Subnet vpcid create next)) = do
    let v = create{ EC2.csub_vpcId = vpcid }
    print v
    debug state $ next v
debug state (Free (Val value next)) = do
    print value
    debug state next
debug state (Pure r) = return r


evalResources exprFile ctx@DeployContext{..} = do
    let s = emptyState exprFile
    Right info <- deploymentInfo ctx s
    store <- readTransactionStore "store"
    
    debug store $ rplan info
    return ()

type TransactionStore = Map Text Value

readTransactionStore path = do
    Just store <- (A.decode :: LBS.ByteString -> Maybe TransactionStore) <$> LBS.readFile path
    return store

commit :: TransactionStore -> FilePath -> IO TransactionStore
commit xstore path = do
    LBS.writeFile path $ A.encode xstore
    return xstore

clookup = Map.lookup
cache = Map.insert
