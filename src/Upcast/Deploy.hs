{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards #-}

module Upcast.Deploy where

import qualified Data.Text as T
import Data.Text (Text(..))
import Data.Maybe
import Data.Default

import Upcast.Interpolate (n)

import Upcast.State
import Upcast.Nix
import Upcast.PhysicalSpec

data DeployContext =
    DeployContext { nixops, nixPath, stateFile, deploymentName, key, sshAuthSock :: Text
                  , closuresPath :: String
                  } deriving (Show)

-- sorry guys, you'll have to impersonate me for now
instance Default DeployContext where
    def = DeployContext
          { nixops = "/tank/proger/dev/upcast/nix"
          , nixPath = "sources"
          , stateFile = "deployments.nixops"
          , deploymentName = "staging"
          , key = "id_rsa.tmp"
          , sshAuthSock = "/dev/null"
          , closuresPath = "/tmp/machines/1"
          }


attr Resource{..} = fromJust . flip lookup resourceAList
dattr Deployment{..} = fromJust . flip lookup deploymentAList
