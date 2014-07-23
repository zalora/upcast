{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards #-}

module Main where

import Control.Monad (join)
import Control.Applicative
import Options.Applicative

import System.Directory (canonicalizePath)
import System.FilePath.Posix
import System.Posix.Files (readSymbolicLink)
import System.Posix.Env (getEnvDefault)

import Data.List (intercalate)
import qualified Data.Text as T
import Data.Text (Text(..))
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
import Upcast.Temp
import qualified Upcast.Nixops as Legacy
import Paths_upcast

context :: String -> [String] -> IO DeployContext
context file args = do
    path <- canonicalizePath file
    let ctx = def { expressionFile = T.pack path
                  , stateFile = replaceExtension path "store"
                  }
    nix <- T.pack <$> getDataFileName "nix"
    machinesPath <- randomTempFileName "machines."
    env <- getEnvDefault "UPCAST_NIX_FLAGS" ""
    let ctx' = ctx { nixops = nix
                   , nixArgs = T.concat [T.pack $ intercalate " " args, " ", T.pack env]
                   , closuresPath = machinesPath
                   }
    print ctx'
    return ctx'

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
    pprint info
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


debug file args = do
    ctx@DeployContext{..} <- context file args
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

go file args = do
    ctx'@DeployContext{..} <- context file args
    deploy ctx'

main = join $ customExecParser prefs opts
  where
    prefs = ParserPrefs { prefMultiSuffix = ""
                        , prefDisambiguate = True
                        , prefShowHelpOnError = True
                        , prefBacktrack = True
                        , prefColumns = 80
                        }

    opts = parser `info` header "upcast - infrastructure orchestratrion"

    parser = subparser (command "go" (args go `info` progDesc "execute a deployment") <>
                        command "test" (args debug `info` progDesc "deployment dry-run"))

    args comm = comm <$> argument str exp <*> many (argument str nixArgs)
    exp = metavar "<expression>"
    nixArgs = metavar "nix arguments..."

