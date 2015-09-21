{-# LANGUAGE RecordWildCards #-}

module Upcast.Infra where

import           Control.Exception.Base
import           Control.Lens
import qualified Control.Monad.Trans.AWS as AWS
import           Control.Monad.Trans.Resource

import           Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable (traverse)
import           System.FilePath.Posix (splitFileName)

import           Upcast.IO (expectRight, stderr)
import           Upcast.Infra.Amazonka
import           Upcast.Infra.AmazonkaTypes
import           Upcast.Infra.Machine
import           Upcast.Infra.NixTypes
import           Upcast.Infra.Types
import           Upcast.Shell
import           Upcast.Types

-- | Read files mentioned in userData for each instance.
preReadUserData :: Attrs Ec2instance -> IO (Attrs (Attrs Text))
preReadUserData =
  traverse (\Ec2instance{..} -> traverse (T.readFile . T.unpack) ec2instance_userData)

-- | Read key pairs while we can do IO.
prepareKeyPairs :: Attrs Ec2keypair -> IO (Attrs (Ec2keypair, ByteString))
prepareKeyPairs =
  Map.traverseWithKey $ \_ kp@Ec2keypair{..} -> do
    pubkey <- expectRight $ fgconsume $ exec "ssh-keygen" [ "-f"
                                                          , T.unpack ec2keypair_privateKeyFile
                                                          , "-y"
                                                          ]
    return (kp, pubkey)


evalInfra :: InfraContext -> IO [Machine]
evalInfra InfraContext{..} = do
  let region = validateRegion inc_infras
  let Infras{..} = inc_infras
  userData <- preReadUserData infraEc2instance
  keypairs <- prepareKeyPairs infraEc2keypair

  env <- do
    env <- AWS.newEnv region AWS.Discover
    if not inc_verbose then return env else do
      logger <- AWS.newLogger AWS.Trace stderr
      return (env & AWS.envLogger .~ logger)
  runResourceT $ AWS.runAWST env $ do
    instanceA <- plan name userData keypairs inc_infras
    machines instanceA keypairs
  where
    name = T.pack $ snd $ splitFileName inc_expressionFile
