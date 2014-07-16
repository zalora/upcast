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

install DeployContext{..} machines =
    let args m@Machine{..} = (remote m, closuresPath </> (T.unpack m_hostname))
        remote Machine{..} = Remote (T.unpack m_keyFile) (T.unpack m_publicIp)
    in concat [ fmap (uncurry (nixCopyClosureTo sshAuthSock) . args) machines
              , fmap (ssh' sshAuthSock . uncurry nixSetProfile . args) machines
              , fmap (ssh' sshAuthSock . nixSwitchToConfiguration . fst . args) machines
              ]

deploy ctx@DeployContext{..} = do
    Right info <- deploymentInfo ctx
    machinesA <- evalResources ctx info
    let machines = fmap snd machinesA
    let machineNames = fmap (T.unpack . fst) machinesA
    let keyFiles = fmap m_keyFile machines
    agentSocket <- setupAgentF sshAddKeyFile keyFiles
    let ctx' = ctx { sshAuthSock = T.pack agentSocket } 
    spec <- physicalSpecFile machines
    let build = nixBuildMachines ctx' [spec, T.unpack expressionFile] uuid machineNames closuresPath
    fgrun build
    mapM_ fgrun $ install ctx' machines

main = do
    ctx <- fmap (context . head) getArgs
    deploy ctx
