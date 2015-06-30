module Main where

import           Options.Applicative

import           Upcast.Deploy (nixSystemProfile)
import           Upcast.Environment (nixPath, nixContext, icontext, buildRemote)
import           Upcast.IO
import           Upcast.Infra (evalInfra, debugEvalInfra)
import           Upcast.Infra.Match (matchInfras)
import           Upcast.Install (install)
import           Upcast.Monad
import           Upcast.Outputs (machines2ssh, machines2nix)
import           Upcast.Types

infra :: InfraCli -> IO [Machine]
infra = icontext >=> evalInfra

infraDebug :: InfraCli -> IO ()
infraDebug = icontext >=> debugEvalInfra >=> const (return ())

infraDump :: InfraCli -> IO ()
infraDump = icontext >=> print . inc_infras

infraNix :: InfraCli -> IO ()
infraNix = infra >=> putStrLn . machines2nix

infraScan :: InfraCli -> IO ()
infraScan = icontext >=> matchInfras . inc_infras >=> pprint

sshConfig :: InfraCli -> IO ()
sshConfig = infra >=> putStrLn . machines2ssh

printNixPath :: IO ()
printNixPath = nixPath >>= putStrLn

main :: IO ()
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

    exp = metavar "<expression file>"

    opts = subparser cmds `info` header "upcast - infrastructure orchestratrion"

    cmds = command "infra"
           (sshConfig <$> infraCliArgs `info`
            progDesc "evaluate infrastructure and output ssh_config(5)")

        <> command "infra-tree"
           (infraDump <$> infraCliArgs `info`
            progDesc "dump infrastructure tree in json format")

        <> command "infra-debug"
           (infraDebug <$> infraCliArgs `info`
            progDesc "evaluate infrastructure in debug mode")

        <> command "infra-nix"
           (infraNix <$> infraCliArgs `info`
            progDesc "evaluate infrastructure and print the nix description")

        <> command "infra-scan"
           (infraScan <$> infraCliArgs `info`
            progDesc "scan for existing resources ignoring the state file")

        <> command "build-remote"
           ((putStrLn <=< buildRemote) <$> buildRemoteCli `info`
            progDesc "forward nix-build to a remote host")

        <> command "nix-path"
           (pure printNixPath `info`
            progDesc "print effective path to upcast nix expressions")

        <> command "install"
           (install <$> installCli `info`
            progDesc "copy a store path closure and set it to a profile")

    infraCliArgs = InfraCli
      <$> optional (strOption
                    (long "state"
                     <> short 's'
                     <> metavar "FILE"
                     <> help "use FILE as state file"))
      <*> argument str exp

    installCli = Install
      <$> (Remote <$> strOption (long "target"
                                 <> short 't'
                                 <> metavar "ADDRESS"
                                 <> help "SSH-accessible host with Nix"))
      <*> (strOption (long "profile"
                      <> short 'p'
                      <> metavar "PROFILE"
                      <> help "set STORE_PATH to PROFILE (otherwise system)")
           <|> pure nixSystemProfile)
      <*> optional (strOption
                    (long "ssh-config"
                     <> short 'c'
                     <> metavar "FILE"
                     <> help "use FILE as ssh_config(5)"))
      <*> (Pull <$> strOption (long "pull"
                               <> short 'f'
                               <> metavar "FROM"
                               <> help "pull store paths from host")
           <|> pure Push)
      <*> argument str (metavar "STORE_PATH")

    buildRemoteCli = BuildRemote
      <$> strOption (long "target"
                    <> short 't'
                    <> metavar "ADDRESS"
                    <> help "SSH-accessible host with Nix")
      <*> optional (strOption (short 'A'
                     <> metavar "ATTRIBUTE"
                     <> help "build a specific attribute in the expression file"))
      <*> switch (long "print"
                  <> short 'p'
                  <> help "cat the derivation output file after build")
      <*> optional (strOption (short 'i'
                     <> metavar "PROFILE"
                     <> help "set the output store path to PROFILE on the target"))
      <*> argument str exp
