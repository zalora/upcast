{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards #-}

module Main where

import Control.Monad (join)
import Control.Arrow ((>>>))
import Control.Applicative
import Options.Applicative
import Control.Concurrent.Async

import System.Directory (canonicalizePath)
import System.FilePath.Posix
import System.Posix.Files (readSymbolicLink)
import System.Posix.Env (getEnvDefault, getEnv)
import System.IO

import Data.List (intercalate)
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.ByteString.Char8 (split)
import Data.Default
import Data.Maybe (catMaybes)

import Upcast.Interpolate (nl)
import Upcast.Nix
import Upcast.Aws
import Upcast.Types
import Upcast.Resource
import Upcast.Deploy
import Upcast.DeployCommands
import Upcast.Command
import Upcast.Temp
import Paths_upcast

sequenceMaybe :: Monad m => [m (Maybe a)] -> m (Maybe a)
sequenceMaybe [] = return Nothing
sequenceMaybe (act:actions) = act >>= maybe (sequenceMaybe actions) (return . Just)

context :: String -> [String] -> IO DeployContext
context file args = do
    path <- canonicalizePath file
    let ctx = def { expressionFile = T.pack path
                  , stateFile = replaceExtension path "store"
                  }
    Just nix <- fmap T.pack <$> sequenceMaybe [getEnv "NIX_UPCAST", Just <$> getDataFileName "nix"]
    machinesPath <- randomTempFileName "machines."
    env <- getEnvDefault "UPCAST_NIX_FLAGS" ""
    let ctx' = ctx { upcastNix = nix
                   , nixArgs = T.concat [T.pack $ intercalate " " args, " ", T.pack env]
                   , closuresPath = machinesPath
                   }
    return ctx'

install DeployContext{..} machines = do
    installs <- mapM installP machines
    mapConcurrently go installs >> return ()
  where
    fgrun' arg = do ExitSuccess <- fgrun arg; return ()

    installP :: Machine -> IO Install
    installP i_machine@Machine{..} = do
      i_closure <- readSymbolicLink $ closuresPath </> (T.unpack m_hostname)
      i_paths <- (fmap (split '\n') . fgconsume . nixClosure) $ i_closure
      return Install{..}
      where
        i_remote = Remote (T.unpack <$> m_keyFile) (T.unpack m_publicIp)

    go :: Install -> IO ()
    go install = mapM_ fgrun' commands
      where
        commands = [ ssh' sshAuthSock . nixTrySubstitutes
                   , nixCopyClosureTo sshAuthSock
                   , ssh' sshAuthSock . nixSetProfile
                   , ssh' sshAuthSock . nixSwitchToConfiguration
                   ] <*> pure install

resources ctx = do
    Right info <- deploymentInfo ctx
    machines <- evalResources ctx info
    return machines

deploy ctx@DeployContext{..} = do
    machines <- resources ctx
    ctx' <- ctxAuth $ catMaybes $ fmap m_keyFile machines
    let build = nixBuildMachines ctx' (T.unpack expressionFile) uuid closuresPath
    ExitSuccess <- fgrun build
    install ctx' machines
  where
    ctxAuth keyFiles = do
      userAuthSock <- getEnv "UPCAST_SSH_AUTH_SOCK"
      agentSocket <- case userAuthSock of
                       Just sock -> return sock
                       Nothing -> setupAgentF sshAddKeyFile keyFiles
      return ctx { sshAuthSock = T.pack agentSocket }

infoOnly file args = do
    Right info <- context file args >>= deploymentInfo
    pprint info

debug file args = do
    ctx@DeployContext{..} <- context file args
    Right info <- deploymentInfo ctx
    machines <- debugEvalResources ctx info
    return ()

buildOnly file args = do
    ctx@DeployContext{..} <- context file args
    let build = nixBuildMachines ctx (T.unpack expressionFile) uuid closuresPath
    fgrun build
    return ()

run file args = do
    ctx@DeployContext{..} <- context file args
    deploy ctx

sshConfig file args = context file args >>= resources >>= putStrLn . intercalate "\n" . fmap config
  where
    config Machine{..} = [nl|
Host #{m_hostname}
    # #{m_instanceId}
    HostName #{m_publicIp}
    User root#{case m_keyFile of Just file -> T.concat ["\n    IdentityFile ", file, "\n"]; Nothing -> ""}
    ControlMaster auto
    ControlPath ~/.ssh/master-%r@%h:%p
    ForwardAgent yes
    ControlPersist 60s
|]


main = do
    hSetBuffering stderr LineBuffering
    join $ customExecParser prefs opts
  where
    prefs = ParserPrefs { prefMultiSuffix = ""
                        , prefDisambiguate = True
                        , prefShowHelpOnError = True
                        , prefBacktrack = True
                        , prefColumns = 80
                        }

    opts = parser `info` header "upcast - infrastructure orchestratrion"

    parser = subparser (command "run" (args run `info` progDesc "evaluate resources, run builds and deploy") <>
                        command "build" (args buildOnly `info` progDesc "perform a build of all machine closures") <>
                        command "ssh-config" (args sshConfig `info` progDesc "dump ssh config for deployment (evaluates resources)") <>
                        command "resource-info" (args infoOnly `info` progDesc "dump resource information in json format") <>
                        command "resource-debug" (args debug `info` progDesc "evaluate resources in debugging mode")
                        )

    args comm = comm <$> argument str exp <*> many (argument str nixArgs)
    exp = metavar "<expression>"
    nixArgs = metavar "nix arguments..."

