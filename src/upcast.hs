{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards #-}

module Main where

import Control.Monad (join, when, unless)
import Control.Arrow ((>>>))
import Control.Applicative
import Options.Applicative
import Control.Concurrent.Async

import System.Directory (canonicalizePath)
import System.FilePath.Posix
import System.Posix.Files (readSymbolicLink)
import System.Posix.Env (getEnvDefault, getEnv)

import Data.List (intercalate)
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.ByteString.Char8 (split)
import Data.Maybe (catMaybes)

import Upcast.IO
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

nixPath :: IO (Maybe String)
nixPath = sequenceMaybe [getEnv "NIX_UPCAST", Just <$> getDataFileName "nix"]

context :: String -> [String] -> IO DeployContext
context file args = do
    path <- canonicalizePath file
    env <- getEnvDefault "UPCAST_NIX_FLAGS" ""

    closuresPath <- randomTempFileName "machines."
    Just upcastNix <- fmap T.pack <$> nixPath
    nixSSHClosureCache <- getEnv "UPCAST_SSH_CLOSURE_CACHE"

    let uuid = "new-upcast-deployment"
        sshAuthSock = "/dev/null"
        nixArgs = T.concat [T.pack $ intercalate " " args, " ", T.pack env]
        expressionFile = T.pack path
        stateFile = replaceExtension path "store"

    return DeployContext{..}

install DeployContext{..} machines = do
    installs <- mapM installP machines
    mapConcurrently go installs >> return ()
  where
    fgrun' = expect ExitSuccess "install step failed" . fgrun

    installP :: Machine -> IO Install
    installP i_machine@Machine{..} = do
      i_closure <- readSymbolicLink $ closuresPath </> (T.unpack m_hostname)
      i_paths <- (fmap (split '\n') . fgconsume_ . nixClosure) $ i_closure
      let i_sshClosureCache = fmap (Remote Nothing) nixSSHClosureCache
      return Install{..}
      where
        i_remote = Remote (T.unpack <$> m_keyFile) (T.unpack m_publicIp)

    baseCommands = [ ssh' sshAuthSock . nixTrySubstitutes
                   , nixCopyClosureTo sshAuthSock
                   , ssh' sshAuthSock . nixSetProfile
                   , ssh' sshAuthSock . nixSwitchToConfiguration
                   ]

    go :: Install -> IO ()
    go install@Install{i_sshClosureCache = Just (Remote _ cacheHost)} =
      mapM_ fgrun' $ (ssh' sshAuthSock . sshPrepCacheKnownHost):baseCommands <*> pure install

    go install =
      mapM_ fgrun' $ baseCommands <*> pure install

resources ctx = do
    info <- expectRight $ deploymentInfo ctx
    machines <- evalResources ctx info
    return machines

deploy ctx@DeployContext{..} = do
    machines <- resources ctx
    let nixMachines = filter (\Machine{..} -> m_nix == True) machines
    unless (null nixMachines) $ do
      ctx' <- ctxAuth $ catMaybes $ fmap m_keyFile nixMachines
      let build = nixBuildMachines ctx' (T.unpack expressionFile) uuid closuresPath
      expect ExitSuccess "nix build of machine closures failed" $ fgrun build
      install ctx' nixMachines
  where
    ctxAuth keyFiles = do
      userAuthSock <- getEnv "UPCAST_SSH_AUTH_SOCK"
      agentSocket <- case userAuthSock of
                       Just sock -> do
                          warn ["Using UPCAST_SSH_AUTH_SOCK: ", sock]
                          return sock
                       Nothing | null keyFiles ->  fallback
                               | otherwise -> setupAgentF sshAddKeyFile keyFiles

      return ctx { sshAuthSock = T.pack agentSocket }

    fallback = do
      sock <- getEnvDefault "SSH_AUTH_SOCK" ""
      warn ["None of instances reference ssh key files, using SSH_AUTH_SOCK (", show sock, ")."]
      when (null sock) $ fail "SSH_AUTH_SOCK is not set, please setup your ssh agent with necessary keys."
      return sock

infoOnly file args = do
    info <- expectRight $ context file args >>= deploymentInfo
    pprint info

debug file args = do
    ctx@DeployContext{..} <- context file args
    info <- expectRight $ deploymentInfo ctx
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

printNixPath = do
  Just p <- nixPath
  putStrLn p

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

    opts = (subparser commands) `info` header "upcast - infrastructure orchestratrion"

    commands = command "run" (args run `info` progDesc "evaluate resources, run builds and deploy")
            <> command "build" (args buildOnly `info` progDesc "perform a build of all machine closures")
            <> command "ssh-config" (args sshConfig `info` progDesc "dump ssh config for deployment (evaluates resources)")
            <> command "resource-info" (args infoOnly `info` progDesc "dump resource information in json format")
            <> command "resource-debug" (args debug `info` progDesc "evaluate resources in debugging mode")
            <> command "nix-path" (pure printNixPath `info` progDesc "print effective path to upcast nix expressions")

    args comm = comm <$> argument str exp <*> many (argument str nixArgs)
    exp = metavar "<expression>"
    nixArgs = metavar "nix arguments..."

