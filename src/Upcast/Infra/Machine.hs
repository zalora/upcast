{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Upcast.Infra.Machine
( machines
) where

import Control.Applicative ((<$>))
import Control.Lens -- (*)
import Control.Monad.State (gets)
import Data.List (find)
import Data.Witherable (catMaybes)
import qualified Network.AWS.EC2.Types as EC2 -- (*)
import Upcast.Infra.NixTypes (Ec2keypair(..))
import Upcast.Infra.Types (AWS, State(..))
import Upcast.Types (Machine(..))

machines :: AWS m => m [Machine]
machines = fmap catMaybes . map . toMachine <$> gets stateKeyPairs >>= (<$> gets stateInstances)

toMachine :: [Ec2keypair] -> EC2.Instance -> Maybe Machine
toMachine keypairs inst = do
  m_hostname   <- inst ^? EC2.insTags . folded . filtered ((== "Name") . view EC2.tagKey) . EC2.tagValue
  m_publicIp   <- inst ^. EC2.insPublicIPAddress
  m_privateIp  <- inst ^. EC2.insPrivateIPAddress
  m_instanceId <- inst ^. EC2.insInstanceId & return
  m_keyFile    <- inst ^. EC2.insKeyName <&> \n -> find ((== n) . ec2keypair_name) keypairs
                                         <&> ec2keypair_privateKeyFile
  return Machine{..}
