{-# LANGUAGE RecordWildCards #-}

module Upcast.Infra where

import           Control.Exception.Base
import           Control.Lens
import qualified Control.Monad.Trans.AWS as AWS

import           Network.AWS.Data (Base64(..))

import qualified Data.ByteString.Base64 as Base64
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable (traverse)
import           System.FilePath.Posix (splitFileName)

import           Upcast.IO (expectRight, stderr)
import           Upcast.Infra.Amazonka
import           Upcast.Infra.AmazonkaTypes
import           Upcast.Infra.NixTypes
import           Upcast.Infra.Types
import           Upcast.Shell
import           Upcast.Types

-- | Read files mentioned in userData for each instance.
preReadUserData :: Attrs Ec2instance -> IO (Attrs (Attrs Text))
preReadUserData =
  traverse (\Ec2instance{..} -> traverse (T.readFile . T.unpack) ec2instance_userData)

-- | Encode Base64 values for key pairs while we can do IO.
prepareKeyPairs :: Attrs Ec2keypair -> IO (Attrs (Ec2keypair, Base64))
prepareKeyPairs =
  Map.traverseWithKey $ \_ kp@Ec2keypair{..} -> do
    pubkey <- expectRight $ fgconsume $ exec "ssh-keygen" [ "-f"
                                                          , T.unpack ec2keypair_privateKeyFile
                                                          , "-y"
                                                          ]
    return (kp, Base64 (Base64.encode pubkey))


evalInfra :: InfraContext -> IO [Machine]
evalInfra InfraContext{..} = do
  let region = validateRegion inc_infras
  let Infras{..} = inc_infras
  userData <- preReadUserData infraEc2instance
  keypairs <- prepareKeyPairs infraEc2keypair

  env <- case inc_verbose of
           False -> AWS.getEnv region AWS.Discover
           True -> do
             env <- AWS.getEnv region AWS.Discover
             logger <- AWS.newLogger AWS.Debug stderr
             return (env & AWS.envLogger .~ logger)
  result <- AWS.runAWST env (plan name userData keypairs inc_infras)
  case result of
    Left e -> throwIO e
    Right m -> return m
  where
    name = T.pack $ snd $ splitFileName inc_expressionFile
