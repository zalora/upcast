module Upcast.Types where

import Data.Text (Text)
import Data.Aeson (Value)
import Data.ByteString.Char8 (ByteString)
import Upcast.Infra.Input (Infras(..))

newtype Remote = Remote String
                 deriving Show

type StorePath = FilePath
type StorePathBS = ByteString
type Hostname = Text
type Executable = String

data NixContext =
  NixContext
  { nix_expressionFile :: FilePath
  , nix_args :: [String]
  } deriving (Show)

data InfraContext = InfraContext
  { inc_expressionFile :: String
  , inc_infras :: Infras
  , inc_verbose :: Bool
  }

-- | Structure used to pass arguments between evaluation and installation phases.
data Machine =
  Machine
  { m_hostname :: Hostname
  , m_publicIp :: Text
  , m_privateIp :: Text
  , m_instanceId :: Text
  , m_keyFile :: Maybe Text
  } deriving (Show)


-- | Per-machine Nix closure install context used during 'upcast install'.
data Install =
  Install
  { i_remote :: Remote
  , i_profile :: FilePath
  , i_sshConfig :: Maybe FilePath
  , i_delivery :: DeliveryMode
  , i_storepath :: StorePath
  } deriving (Show)

data DeliveryMode = Push | Pull String
                  deriving Show

-- | CLI arguments to 'infra*'.
data InfraCli =
  InfraCli
  { infraCli_expressionFile :: FilePath
  , infraCli_verbose :: Bool
  , infraCli_extra :: [String]
  } deriving (Show)

-- | Arguments to 'build'.
data Build =
  Build
  { b_builder :: String
  , b_attribute :: Maybe String
  , b_cat :: Bool
  , b_installProfile :: Maybe FilePath
  , b_expressionFile :: FilePath
  , b_extra :: [String]
  } deriving (Show)
