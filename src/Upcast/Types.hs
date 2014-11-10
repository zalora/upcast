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

data EnvContext = EnvContext
                { upcastNix :: Text
                , nixArgs :: Text
                , nixSSHClosureCache :: Maybe String
                } deriving (Show)

-- | Structure used to carry user (commandline & enviroment) and runtime (currently ssh agent) globals.
data DeployContext = DeployContext
                  { expressionFile :: String
                  , stateFile :: FilePath -- ^ *.store file
                  , uuid :: String -- ^ Used by nix files, NixOps legacy.
                  , envContext :: EnvContext
                  } deriving (Show)

data InfraContext = InfraContext
                  { inc_expressionFile :: String
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
             , m_nix :: Bool
             } deriving (Show)

-- | Per-machine Nix closure install context used in some of 'DeployCommands'.
data Install = Install
             { i_remote :: Remote
             , i_closure :: StorePath
             , i_paths :: [StorePathBS] -- ^ all deps of 'i_closure' (for 'nixTrySubstitutes')
             , i_profile :: FilePath
             } deriving (Show)

-- | CLI arguments to 'install'.
data InstallCli = InstallCli
                { ic_target :: String
                , ic_profile :: Maybe FilePath
                , ic_pullFrom :: Maybe String
                , ic_closure :: FilePath
                } deriving (Show)

-- | CLI arguments to 'run'.
data RunCli = RunCli
            { rc_closureSubstitutes :: Maybe (Map Hostname StorePath)
            , rc_pullFrom :: Maybe String
            , rc_expressionFile :: FilePath
            }

