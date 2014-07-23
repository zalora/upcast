{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Upcast.Types where

import System.FilePath (FilePath)
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.Default

data DeployContext =
    DeployContext { upcastNix, nixArgs, expressionFile, sshAuthSock :: Text
                  , closuresPath, uuid :: String
                  , stateFile :: FilePath
                  } deriving (Show)

instance Default DeployContext where
    def = DeployContext
          { upcastNix = ""
          , nixArgs = ""
          , stateFile = ""
          , expressionFile = ""
          , sshAuthSock = "/dev/null"
          , closuresPath = "/tmp/machines/1"
          , uuid = "new-upcast-deployment"
          }

