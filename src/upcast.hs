{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards #-}

module Main where

import System.FilePath.Posix
import System.Posix.Files (readSymbolicLink)
import System.Environment (getArgs)
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString.Char8 (split)

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

install DeployContext{..} machines = do
    closures <- mapM (fmap (split '\n') . fgconsume . nixClosure . closurePath) machines
    sysEnv <- mapM (readSymbolicLink . closurePath) machines
    let margs = zip (fmap remote machines) sysEnv
    mapM_ fgrun $ commands margs closures
  where
    closurePath Machine{..} = closuresPath </> (T.unpack m_hostname)
    remote Machine{..} = Remote (T.unpack m_keyFile) (T.unpack m_publicIp)

    clargs (cls, m) = (remote m, cls)

    commands margs cls = concat [ fmap (ssh' sshAuthSock . uncurry nixTrySubstitutes . clargs) $ zip cls machines
                                , fmap (uncurry (nixCopyClosureTo sshAuthSock)) margs
                                , fmap (ssh' sshAuthSock . uncurry nixSetProfile) margs
                                , fmap (ssh' sshAuthSock . nixSwitchToConfiguration) $ fmap remote machines
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
    install ctx' machines

debug = do
    ctx@DeployContext{..} <- fmap (context . head) getArgs
    Right info <- deploymentInfo ctx
    pprint info
    machinesA <- debugEvalResources ctx info

    let machineNames = fmap (T.unpack . fst) machinesA
    spec <- physicalSpecFile $ fmap snd machinesA
    let build = nixBuildMachines ctx [spec, T.unpack expressionFile] uuid machineNames closuresPath
    fgrun build

    -- infoOnly ctx
    -- deploy ctx
    return ()

main = do
    ctx@DeployContext{..} <- fmap (context . head) getArgs
    deploy ctx
