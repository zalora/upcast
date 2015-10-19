module Main where

import           Control.Monad (void)
import           Options.Applicative
import           Upcast.Deploy (nixSystemProfile)
import           Upcast.Environment (nixPath, icontext, build)
import           Upcast.IO
import qualified Upcast.Infra as Infra (scan, accum, dump)
import           Upcast.Infra.Types (Reference(..))
import           Upcast.Install (install)
import           Upcast.Monad
import           Upcast.Outputs (machines2ssh, machines2nix)
import           Upcast.Types

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
    nixArgs = many (argument str (metavar "nix arguments..."))

    opts = subparser cmds `info` header "upcast - infrastructure orchestration"

    cmds = command "infra"
           ((putStrLn . machines2ssh <=< Infra.accum <=< icontext) <$> infraCliArgs `info`
            progDesc "evaluate infrastructure and output ssh_config(5)")

        <> command "infra-tree"
           ((void . Infra.dump (\(Reference _ k) i -> putStrLn $ ppShow (k,i)) <=< icontext) <$> infraCliArgs `info`
            progDesc "dump infrastructure tree in json format")

        <> command "infra-nix"
           ((putStr . machines2nix <=< Infra.accum <=< icontext) <$> infraCliArgs `info`
            progDesc "evaluate infrastructure and print the nix description")

        <> command "infra-scan"
           ((pprint <=< Infra.scan <=< icontext) <$> infraCliArgs `info`
            progDesc "scan for existing infra")

        <> command "build"
           ((putStrLn <=< build) <$> buildCli `info`
            progDesc "nix-build with remote forwarding")

        <> command "nix-path"
           (pure (nixPath >>= putStrLn) `info`
            progDesc "print effective path to upcast nix expressions")

        <> command "install"
           (install <$> installCli `info`
            progDesc "copy a store path closure and set it to a profile")

    infraCliArgs = InfraCli
      <$> argument str exp
      <*> (switch (long "verbose" <> short 'v' <> help "enable extra logging") <|> pure False)
      <*> nixArgs

    installCli = Install
      <$> (Remote <$> (strOption (long "target"
                                 <> short 't'
                                 <> metavar "ADDRESS"
                                 <> help "SSH-accessible host with Nix")
                       <|> pure "localhost"))
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
                               <> help "pull store paths from host (relative to ADDRESS)")
           <|> pure Push)
      <*> argument str (metavar "STORE_PATH")

    buildCli = Build
      <$> (strOption (long "target"
                    <> short 't'
                    <> metavar "ADDRESS"
                    <> help "SSH-accessible host with Nix") <|> pure "localhost")
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
      <*> nixArgs
