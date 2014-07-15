{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Upcast.Types where

import System.FilePath (FilePath)
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.Default

data DeployContext =
    DeployContext { nixops, nixPath, expressionFile, sshAuthSock :: Text
                  , closuresPath, uuid :: String
                  , stateFile :: FilePath
                  } deriving (Show)

-- sorry guys, you'll have to impersonate me for now
instance Default DeployContext where
    def = DeployContext
          { nixops = "/tank/proger/dev/upcast/nix"
          , nixPath = "sources"
          , stateFile = ""
          , expressionFile = ""
          , sshAuthSock = "/dev/null"
          , closuresPath = "/tmp/machines/1"
          , uuid = "new-upcast-deployment"
          }

