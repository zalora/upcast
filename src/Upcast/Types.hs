{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Upcast.Types where

import System.FilePath (FilePath)
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.ByteString.Char8 (ByteString)
import Data.Map (Map)

import Upcast.Command (Remote)

data DeployMode = Default | Unattended deriving (Eq, Show)

type StorePath = String
type StorePathBS = ByteString

data EnvContext = EnvContext
                { upcastNix :: Text
                , nixArgs :: Text
                , nixSSHClosureCache :: Maybe String
                , deployMode :: DeployMode
                , sshAuthSock :: Text
                } deriving (Show)

-- | Structure used to carry user (commandline & enviroment) and runtime (currently ssh agent) globals.
data DeployContext = DeployContext
                  { closuresPath :: String -- ^ Path to store links to machine closures.
                  , expressionFile :: String
                  , stateFile :: FilePath -- ^ *.store file
                  , uuid :: String -- ^ Used by nix files, NixOps legacy.
                  , closureSubstitutes :: Map Text StorePath
                  , envContext :: EnvContext
                  } deriving (Show)

-- | Structure used to pass arguments between evaluation and installation phases.
data Machine = Machine
             { m_hostname :: Text
             , m_publicIp :: Text
             , m_privateIp :: Text
             , m_instanceId :: Text
             , m_keyFile :: Maybe Text
             , m_nix :: Bool
             } deriving (Show)

-- | Per-machine Nix closure install context used in some of 'DeployCommands'.
data Install = Install
             { i_remote :: Remote
             , i_closure :: StorePath
             , i_paths :: [StorePathBS]
             , i_sshClosureCache :: Maybe Remote
             } deriving (Show)

-- | CLI arguments to the 'install' command.
data InstallCli = InstallCli
                { ic_target :: String
                , ic_closure :: FilePath
                } deriving (Show)

