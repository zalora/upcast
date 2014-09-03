{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Upcast.Types where

import System.FilePath (FilePath)
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.ByteString.Char8 (ByteString)

import Upcast.Command (Remote)

-- | Structure used to carry user (commandline & enviroment) and runtime (currently ssh agent) globals.
data DeployContext =
    DeployContext { upcastNix :: Text
                  , nixArgs :: Text
                  , sshAuthSock :: Text
                  , closuresPath :: String -- ^ Path to store links to machine closures.
                  , expressionFile :: Text
                  , stateFile :: FilePath -- ^ *.store file
                  , uuid :: String -- ^ Used by nix files, NixOps legacy.
                  , nixSSHClosureCache :: Maybe String
                  } deriving (Show)

-- | Structure used to pass arguments between evaluation and installation phases.
data Machine = Machine
             { m_hostname :: Text
             , m_publicIp :: Text
             , m_privateIp :: Text
             , m_instanceId :: Text
             , m_keyFile :: Maybe Text
             } deriving (Show)

type StorePath = String
type StorePathBS = ByteString

-- | Per-machine Nix closure install context used in some of 'DeployCommands'.
data Install = Install
             { i_machine :: Machine
             , i_remote :: Remote
             , i_closure :: StorePath
             , i_paths :: [StorePathBS]
             , i_sshClosureCache :: Maybe Remote
             } deriving (Show)
