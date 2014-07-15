{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards #-}

module Main where

import System.FilePath.Posix
import System.Environment (getArgs)
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Default

import Upcast.Interpolate (n)

import Upcast.Nix
import Upcast.PhysicalSpec
import Upcast.Aws
import Upcast.Types

import Upcast.Resource
import Upcast.Deploy
import Upcast.DeployCommands
import Upcast.Command

import qualified Upcast.Nixops as Legacy

context :: FilePath -> DeployContext
context exprFile = def { expressionFile = T.pack exprFile
                       , stateFile = replaceExtension exprFile "store"
                       }
infoOnly ctx = do
    Right info <- deploymentInfo ctx
    pprint info

deployPlan ctx@DeployContext{..} = do
    machinesA <- evalResources ctx
    spec <- physicalSpecFile $ fmap snd machinesA
    return $ nixBuildMachines ctx [spec, T.unpack expressionFile] uuid (fmap (T.unpack . fst) machinesA) closuresPath

main = fmap (context . head) getArgs >>= deployPlan >>= fgrun
