{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards #-}

module Main where

import Upcast.Monad
import Options.Applicative

import System.Directory (removeFile)
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
import Upcast.Infra
import Upcast.DeployCommands
import Upcast.Command
import Upcast.Temp
import Upcast.Environment
import Upcast.Install

deploymentInfo :: DeployContext -> IO (Either String Value)
deploymentInfo ctx =
    let info = nixDeploymentInfo ctx (expressionFile ctx) (uuid ctx)
        in do
          i <- fgconsume_ info
          return $ nixValue i

infra ctx = do
    info <- expectRight $ deploymentInfo ctx
    machines <- evalInfra ctx info
    return machines

deploy ctx@DeployContext{envContext=EnvContext{..}, ..} = do
    machines <- infra ctx
    let nixMachines = filter (\Machine{..} -> m_nix == True) machines
    unless (null nixMachines) $ do
      ctx' <- ctxAuth ctx $ catMaybes $ fmap m_keyFile nixMachines
      when (deployMode /= Unattended) $ do
        let build = nixBuildMachines ctx' expressionFile uuid
        expect ExitSuccess "nix build of machine closures failed" $ fgrun build
      installMachines ctx' nixMachines

run file = context file >>= deploy

infraDump file = do
    info <- expectRight $ context file >>= deploymentInfo
    pprint info

infraDebug file = do
    ctx@DeployContext{..} <- context file
    info <- expectRight $ deploymentInfo ctx
    machines <- debugEvalInfra ctx info
    return ()

build file = do
    ctx@DeployContext{..} <- context file
    expect ExitSuccess "build failed" $
      fgrun $ nixBuildMachines ctx expressionFile uuid

instantiate file = do
    ctx@DeployContext{..} <- context file
    tmp <- randomTempFileName "drvlink."
    expect ExitSuccess "instantiation failed" $ do
      fgrun $ nixInstantiateMachines ctx expressionFile uuid tmp
    drvPath <- readSymbolicLink tmp
    removeFile tmp
    putStrLn drvPath

sshConfig file =
    context file >>= infra >>= putStrLn . intercalate "\n" . fmap config
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
           (args run `info`
            progDesc "evaluate infrastructure, run builds and deploy")

        <> command "infra"
           (args sshConfig `info`
            progDesc "evaluate infrastructure and output ssh_config(5)")

        <> command "infra-tree"
           (args infraDump `info`
            progDesc "dump infrastructure tree in json format")

        <> command "infra-debug"
           (args infraDebug `info`
            progDesc "evaluate infrastructure in debug mode")

        <> command "instantiate"
           (args instantiate `info`
            progDesc "nix-instantiate all NixOS closures")

        <> command "build"
           (args build `info`
            progDesc "nix-build all NixOS closures")

        <> command "nix-path"
           (pure printNixPath `info`
            progDesc "print effective path to upcast nix expressions")

        <> command "install"
           (install <$> installCli `info`
            progDesc "install nix environment-like closure over ssh")

    installCli :: Parser InstallCli
    installCli = InstallCli
                 <$> strOption (long "target"
                                <> short 't'
                                <> metavar "ADDRESS"
                                <> help "SSH-accessible host with Nix")
                 <*> optional (strOption
                               (long "profile"
                                <> short 'p'
                                <> metavar "PROFILE"
                                <> help "attach CLOSURE to PROFILE (otherwise system)"))
                 <*> argument str (metavar "CLOSURE")
