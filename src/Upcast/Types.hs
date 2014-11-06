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

-- | Structure used to carry user (commandline & enviroment) and runtime (currently ssh agent) globals.
data DeployContext =
    DeployContext { upcastNix :: Text
                  , nixArgs :: Text
                  , sshAuthSock :: Text
                  , closuresPath :: String -- ^ Path to store links to machine closures.
                  , expressionFile :: String
                  , stateFile :: FilePath -- ^ *.store file
                  , uuid :: String -- ^ Used by nix files, NixOps legacy.
                  , nixSSHClosureCache :: Maybe String
                  , deployMode :: DeployMode
                  , closureSubstitutes :: Map Text StorePath
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
             { i_machine :: Machine
             , i_remote :: Remote
             , i_closure :: StorePath
             , i_paths :: [StorePathBS]
             , i_sshClosureCache :: Maybe Remote
             } deriving (Show)
