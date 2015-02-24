{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Upcast.Types where

import System.FilePath (FilePath)
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.Aeson (Value)
import Data.ByteString.Char8 (ByteString)
import Data.Map (Map)

import Upcast.Command (Remote)

type StorePath = String
type StorePathBS = ByteString
type Hostname = Text

data NixContext = NixContext
                  { nix_expressionFile :: FilePath
                  , nix_args :: Text
                  , nix_sshStoreCache :: Maybe String
                  } deriving (Show)

data InfraContext = InfraContext
                  { inc_expressionFile :: String
                  , inc_realmName :: Text
                  , inc_data :: Value
                  , inc_stateFile :: FilePath
                  } deriving (Show)

-- | Structure used to pass arguments between evaluation and installation phases.
data Machine = Machine
             { m_hostname :: Hostname
             , m_publicIp :: Text
             , m_privateIp :: Text
             , m_instanceId :: Text
             , m_keyFile :: Maybe Text
             } deriving (Show)

-- | Per-machine Nix closure install context used in some of 'DeployCommands'.
data Install = Install
             { i_remote :: Remote
             , i_storepath :: StorePath
             , i_paths :: [StorePathBS] -- ^ all deps of 'i_storepath' (for 'nixTrySubstitutes')
             , i_profile :: FilePath
             } deriving (Show)

data DeliveryMode = Push | Pull String

toDelivery :: Maybe String -> DeliveryMode
toDelivery = maybe Push Pull

-- | CLI arguments to 'infra*'
data InfraCli = InfraCli
                { infraCli_stateFile :: Maybe FilePath
                , infraCli_expressionFile :: FilePath
                } deriving (Show)

-- | CLI arguments to 'install'.
data InstallCli = InstallCli
                { ic_target :: String
                , ic_profile :: Maybe FilePath
                , ic_sshConfig :: Maybe FilePath
                , ic_pullFrom :: Maybe String
                , ic_storepath :: FilePath
                } deriving (Show)

-- | CLI arguments to 'buildRemote'.
data BuildRemoteCli = BuildRemoteCli
                    { brc_builder :: String
                    , brc_attribute :: Maybe String
                    , brc_cat :: Bool
                    , brc_installProfile :: Maybe FilePath
                    , brc_expressionFile :: FilePath
                    }
