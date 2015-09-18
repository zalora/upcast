{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Upcast.Infra.Machine where

import           Network.AWS.Prelude

import           Control.Lens hiding ((.=))
import           Control.Monad.Trans.AWS (send)
import qualified Network.AWS.EC2 as EC2
import qualified Network.AWS.EC2.Types as EC2
import qualified Data.Map.Strict as Map

import           Upcast.Infra.NixTypes
import           Upcast.Infra.AmazonkaTypes
import           Upcast.Infra.Types
import           Upcast.Types (Machine(..), Hostname)

machines :: AWSC m => [(Hostname, ResourceId)] -> Attrs (Ec2keypair, ByteString) -> m [Machine]
machines [] _ = return []
machines instanceA keypairs = fmap toMachine (send request)
  where
    request = EC2.describeInstances & EC2.diiInstanceIds .~ map fst nameById
    nameById = map (\(k, v) -> (v, k)) instanceA
    keypairFileByName =
      map (\(Ec2keypair{..}, _) -> (ec2keypair_name, ec2keypair_privateKeyFile)) $ Map.elems keypairs

    toMachine resp = do
      rs <- resp ^. EC2.dirsReservations
      inst <- rs ^. EC2.rInstances
      let id = inst ^. EC2.insInstanceId
      let Just name = lookup id nameById
      let Just publicIp = inst ^. EC2.insPublicIPAddress
      let Just privateIp = inst ^. EC2.insPrivateIPAddress
      let keyFile = inst ^. EC2.insKeyName >>= (`lookup` keypairFileByName)
      return $ Machine name publicIp privateIp id keyFile
