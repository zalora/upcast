{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards, NamedFieldPuns #-}

module Main where

import Upcast.Monad
import Options.Applicative

import System.Directory (removeFile)
import System.Posix.Files (readSymbolicLink)
import System.FilePath.Posix

import Data.List (intercalate)
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.Maybe (catMaybes)
import qualified Data.Map as Map

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

evalInfraContext :: DeployContext -> IO InfraContext
evalInfraContext ctx@DeployContext{..} =
    let info = nixDeploymentInfo ctx expressionFile uuid
        in do
          i <- fgconsume_ info
          value <- expectRight $ return (nixValue i)
          return InfraContext{ inc_expressionFile = expressionFile
                             , inc_data = value
                             , inc_stateFile = stateFile
                             }

infra = context >=> evalInfraContext >=> evalInfra

infraDump = context >=> evalInfraContext >=> pprint . inc_data

infraDebug = context >=> evalInfraContext >=> debugEvalInfra >=> const (return ())

maybeBuild machines = go nixMachines
  where
    go [] = return ()
    go _ = return ()
    nixMachines = [m | m@Machine{..} <- machines, m_nix]

run RunCli{..} = do
  ctx@DeployContext{envContext} <- context rc_expressionFile
  machines' <- evalInfra =<< evalInfraContext ctx
  let machines = [m | m@Machine{..} <- machines', m_nix]
  when (null machines) $ oops "no Nix instances, plan complete"

  case rc_closureSubstitutes of
      Nothing ->
        buildThenInstall ctx rc_pullFrom machines
      Just s ->
        installMachines rc_pullFrom
        (maybe (error "closure not found") return . flip Map.lookup s) machines

buildThenInstall ctx pullFrom machines = do
  closuresPath <- randomTempFileName "machines."

  expect ExitSuccess "nix build of machine closures failed" $
    fgrun $ nixBuildMachines ctx $ Just closuresPath

  prepAuth $ catMaybes $ fmap m_keyFile machines
  installMachines pullFrom (readSymbolicLink . (closuresPath </>) . T.unpack) machines

build = context >=> expect ExitSuccess "build failed" .  fgrun . flip nixBuildMachines Nothing

instantiate = context >=> instantiateTmp >=> putStrLn

instantiateTmp ctx = do
  tmp <- randomTempFileName "drvlink."
  expect ExitSuccess "instantiation failed" $ do
    fgrun $ nixInstantiateMachines ctx tmp
  drvPath <- readSymbolicLink tmp
  removeFile tmp
  return drvPath

sshConfig = infra >=> putStrLn . intercalate "\n" . fmap config
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
           (run <$> runCli `info`
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
                 <*> pullOption
                 <*> argument str (metavar "CLOSURE")
