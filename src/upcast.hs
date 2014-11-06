{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards #-}

module Main where

import Upcast.Monad
import Control.Applicative
import Options.Applicative
import Control.Concurrent.Async

import System.Directory (canonicalizePath, removeFile)
import System.FilePath.Posix
import System.Posix.Files (readSymbolicLink)
import System.Posix.Env (getEnvDefault, getEnv)

import Data.List (intercalate)
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.ByteString.Char8 (split)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (catMaybes)
import qualified Data.Map as Map

import Data.Aeson (decodeStrict)

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
    unattended <- getEnv "UPCAST_UNATTENDED"
    subs <- getEnv "UPCAST_CLOSURES"

    let uuid = "new-upcast-deployment"
        sshAuthSock = "/dev/null"
        nixArgs = T.concat [T.pack $ intercalate " " args, " ", T.pack env]
        expressionFile = path
        stateFile = replaceExtension path "store"
        deployMode = maybe Default (const Unattended) unattended
        closureSubstitutes = maybe Map.empty id $ join $ decodeStrict . BS.pack <$> subs

    return DeployContext{..}

install ctx@DeployContext{..} machines = do
    installs <- mapM installP machines
    mapConcurrently go installs
    return ()
  where
    fgrun' = expect ExitSuccess "install step failed" . fgrun
    fgssh = fgrun' . ssh sshAuthSock

    resolveClosure :: Text -> IO StorePath
    resolveClosure hostname =
        case Map.lookup hostname closureSubstitutes of
            Just x -> return x
            _ -> readSymbolicLink $ closuresPath </> (T.unpack hostname)

    installP :: Machine -> IO Install
    installP i_machine@Machine{..} = do
      i_closure <- resolveClosure m_hostname
      i_paths <- (fmap (split '\n') . fgconsume_ . nixClosure) $ i_closure
      let i_sshClosureCache = fmap (Remote Nothing) nixSSHClosureCache
      return Install{..}
      where
        i_remote = Remote (T.unpack <$> m_keyFile) (T.unpack m_publicIp)
  
    go :: Install -> IO ()
    go install@Install{i_sshClosureCache = Just (Remote _ cacheHost)} = do
      fgssh $ sshPrepCacheKnownHost install
      go' install

    go install = go' install

    go' install = do
      if (deployMode /= Unattended)
          then do
            fgssh . nixTrySubstitutes $ install
            fgrun' . nixCopyClosureTo sshAuthSock $ install
          else do
            fgssh . nixCopyClosureFrom $ install
      fgssh . nixSetProfile $ install
      fgssh . nixSwitchToConfiguration $ install


resources ctx = do
    info <- expectRight $ deploymentInfo ctx
    machines <- evalResources ctx info
    return machines

deploy ctx@DeployContext{..} = do
    machines <- resources ctx
    let nixMachines = filter (\Machine{..} -> m_nix == True) machines
    unless (null nixMachines) $ do
      ctx' <- ctxAuth ctx $ catMaybes $ fmap m_keyFile nixMachines
      when (deployMode /= Unattended) $ do
        let build = nixBuildMachines ctx' expressionFile uuid
        expect ExitSuccess "nix build of machine closures failed" $ fgrun build
      install ctx' nixMachines

ctxAuth :: DeployContext -> [Text] -> IO DeployContext
ctxAuth ctx keyFiles = do
    userAuthSock <- getEnv "UPCAST_SSH_AUTH_SOCK"
    agentSocket <- case userAuthSock of
                     Just sock -> do
                        warn ["Using UPCAST_SSH_AUTH_SOCK: ", sock]
                        return sock
                     Nothing | null keyFiles ->  fallback
                             | otherwise -> setupAgentF sshAddKeyFile keyFiles

    return ctx { sshAuthSock = T.pack agentSocket }
  where
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
    expect ExitSuccess "build failed" $
      fgrun $ nixBuildMachines ctx expressionFile uuid

instantiateOnly file args = do
    ctx@DeployContext{..} <- context file args
    tmp <- randomTempFileName "drvlink."
    expect ExitSuccess "instantiation failed" $ do
      fgrun $ nixInstantiateMachines ctx expressionFile uuid tmp
    drvPath <- readSymbolicLink tmp
    removeFile tmp
    putStrLn drvPath

run file args = context file args >>= deploy

sshConfig file args =
    context file args >>= resources >>= putStrLn . intercalate "\n" . fmap config
  where
    identity (Just file) = T.concat ["\n    IdentityFile ", file, "\n"]
    identity Nothing = ""

    config Machine{..} = [nl|
Host #{m_hostname}
    # #{m_instanceId}
    HostName #{m_publicIp}
    User root#{identity m_keyFile}
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

    args comm = comm <$> argument str exp <*> many (argument str nixArgs)
    exp = metavar "<expression>"
    nixArgs = metavar "nix arguments..."

    opts = (subparser cmds) `info` header "upcast - infrastructure orchestratrion"

    cmds = command "run" (args run `info` progDesc "evaluate resources, run builds and deploy")
        <> command "instantiate" (args instantiateOnly `info` progDesc "perform instantiation of all machine closures")
        <> command "build" (args buildOnly `info` progDesc "perform a build of all machine closures")
        <> command "ssh-config" (args sshConfig `info` progDesc "dump ssh config for deployment (evaluates resources)")
        <> command "resource-info" (args infoOnly `info` progDesc "dump resource information in json format")
        <> command "resource-debug" (args debug `info` progDesc "evaluate resources in debugging mode")
        <> command "nix-path" (pure printNixPath `info` progDesc "print effective path to upcast nix expressions")

