{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards #-}

module Main where

import Upcast.Monad
import Options.Applicative

import System.Directory (removeFile)
import System.Posix.Env (getEnvDefault, getEnv)
import System.Posix.Files (readSymbolicLink)

import Data.List (intercalate)
import qualified Data.Text as T
import Data.Text (Text(..))
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
import Upcast.Environment
import Upcast.Install

resources ctx = do
    info <- expectRight $ deploymentInfo ctx
    machines <- evalResources ctx info
    return machines

deploy ctx@DeployContext{envContext=EnvContext{..}, ..} = do
    machines <- resources ctx
    let nixMachines = filter (\Machine{..} -> m_nix == True) machines
    unless (null nixMachines) $ do
      ctx' <- ctxAuth ctx $ catMaybes $ fmap m_keyFile nixMachines
      when (deployMode /= Unattended) $ do
        let build = nixBuildMachines ctx' expressionFile uuid
        expect ExitSuccess "nix build of machine closures failed" $ fgrun build
      installMachines ctx' nixMachines

ctxAuth :: DeployContext -> [Text] -> IO DeployContext
ctxAuth ctx keyFiles = do
    userAuthSock <- getEnv "UPCAST_SSH_AUTH_SOCK"
    agentSocket <- case userAuthSock of
                     Just sock -> do
                        warn ["Using UPCAST_SSH_AUTH_SOCK: ", sock]
                        return sock
                     Nothing | null keyFiles ->  fallback
                             | otherwise -> setupAgentF sshAddKeyFile keyFiles

    return ctx { envContext = (envContext ctx){ sshAuthSock = T.pack agentSocket } }
  where
    fallback = do
      sock <- getEnvDefault "SSH_AUTH_SOCK" ""
      warn ["None of instances reference ssh key files, using SSH_AUTH_SOCK (", show sock, ")."]
      when (null sock) $ fail "SSH_AUTH_SOCK is not set, please setup your ssh agent with necessary keys."
      return sock

infoOnly file = do
    info <- expectRight $ context file >>= deploymentInfo
    pprint info

debug file = do
    ctx@DeployContext{..} <- context file
    info <- expectRight $ deploymentInfo ctx
    machines <- debugEvalResources ctx info
    return ()

buildOnly file = do
    ctx@DeployContext{..} <- context file
    expect ExitSuccess "build failed" $
      fgrun $ nixBuildMachines ctx expressionFile uuid

instantiateOnly file = do
    ctx@DeployContext{..} <- context file
    tmp <- randomTempFileName "drvlink."
    expect ExitSuccess "instantiation failed" $ do
      fgrun $ nixInstantiateMachines ctx expressionFile uuid tmp
    drvPath <- readSymbolicLink tmp
    removeFile tmp
    putStrLn drvPath

run file = context file >>= deploy

sshConfig file =
    context file >>= resources >>= putStrLn . intercalate "\n" . fmap config
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

    args comm = comm <$> argument str exp
    exp = metavar "<expression>"

    opts = (subparser cmds) `info` header "upcast - infrastructure orchestratrion"

    cmds = command "run"
           (args run `info` progDesc "evaluate resources, run builds and deploy")
        <> command "instantiate"
           (args instantiateOnly `info`
             progDesc "perform instantiation of all machine closures")
        <> command "build"
           (args buildOnly `info` progDesc "perform a build of all machine closures")
        <> command "ssh-config"
           (args sshConfig `info`
             progDesc "dump ssh config for deployment (evaluates resources)")
        <> command "resource-info"
           (args infoOnly `info` progDesc "dump resource information in json format")
        <> command "resource-debug"
           (args debug `info` progDesc "evaluate resources in debugging mode")
        <> command "nix-path"
           (pure printNixPath `info`
             progDesc "print effective path to upcast nix expressions")
        <> command "install"
           (install <$> installCli `info` progDesc "install system closure over ssh")

    installCli :: Parser InstallCli
    installCli = InstallCli
                 <$> strOption (long "target"
                                <> short 't'
                                <> metavar "ADDRESS"
                                <> help "SSH-accessible host that contains Nix")
                 <*> argument str (metavar "CLOSURE")
