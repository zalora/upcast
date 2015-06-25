{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ImplicitParams #-}

module Upcast.DeployCommands where

import           Data.Maybe (fromMaybe)
import           Data.Monoid (Monoid, mconcat)
import           Data.String (IsString)
import           Data.Text (Text)

import           Data.ByteString.Char8 (split)
import qualified Data.ByteString.Char8 as C8

import           Upcast.IO (Str, toString, args, squote, env)
import           Upcast.Command (Command(..), Local(..), Remote(..), fgrunDirect, spawn)
import           Upcast.Temp (randomTempFileName)
import           Upcast.Types (StorePath, NixContext(..), Install(..))

sshEnv :: Str a => a -> String
sshEnv socket = env [("SSH_AUTH_SOCK", toString socket), ("SSH_ASKPASS", "/usr/bin/true")]

nixSshEnv :: (?sshConfig :: Maybe FilePath) => String
nixSshEnv = env [("NIX_SSHOPTS", squote sshBaseOptions)]


sshBaseOptions :: (?sshConfig :: Maybe FilePath) => String
sshBaseOptions = args [conf, base]
  where
    base = args [ "-A"
                , "-o", "StrictHostKeyChecking=no"
                , "-o", "UserKnownHostsFile=/dev/null"
                , "-o", "PasswordAuthentication=no"
                , "-o", "PreferredAuthentications=publickey"
                , "-x"
                ]
    conf = case ?sshConfig of
               Nothing -> []
               Just x -> args ["-F", x]

sshAgent :: FilePath -> Command Local
sshAgent socket =
  Cmd Local (args ["ssh-agent", "-a", socket]) "agent"

sshAddKey :: FilePath -> Text -> Command Local
sshAddKey socket key =
  Cmd Local (mconcat [ "echo ", squote key
                     , " | "
                     , sshEnv socket
                     , " ssh-add -"]) "agent"

sshAddKeyFile :: FilePath -> Text -> Command Local
sshAddKeyFile socket keyFile =
  Cmd Local (args [ sshEnv socket, "ssh-add", toString keyFile ]) "agent"

sshListKeys :: FilePath -> Command Local
sshListKeys socket =
  Cmd Local (args [ sshEnv socket, "ssh-add", "-l" ]) "agent"

setupAgent :: [Text] -> IO FilePath
setupAgent = setupAgentF sshAddKey

setupAgentF :: (FilePath -> a -> Command Local) -> [a] -> IO FilePath
setupAgentF liftKey keyvals = do
    agentSocket <- randomTempFileName "ssh-agent.sock."
    spawn $ sshAgent agentSocket
    mapM_ (fgrunDirect . liftKey agentSocket) keyvals
    fgrunDirect $ sshListKeys agentSocket
    return agentSocket

nixInfraInfo :: NixContext -> Command Local
nixInfraInfo NixContext{..} =
    Cmd Local (args [ "nix-instantiate"
                    , toString nix_args
                    , "--argstr", "expr", squote nix_expressionFile
                    , squote "<upcast/eval-infra.nix>"
                    , "--eval-only", "--strict", "--json"
                    , "--read-write-mode"
                    ]) "info"

nixInstantiate :: Str a => a -> Maybe String -> FilePath -> FilePath -> Command Local
nixInstantiate nix_args attr exprFile root =
    Cmd Local (args [ "nix-instantiate"
                    , toString nix_args
                    , "--read-write-mode"
                    , "--add-root", squote root
                    , "--indirect"
                    , attrString
                    , squote exprFile
                    ]) "instantiate"
  where
    attrString = case attr of
                    Nothing -> []
                    Just a -> args ["-A", squote a]

nixRealise :: FilePath -> Command Local
nixRealise drv = Cmd Local (args ["nix-store", "--realise", drv]) "realise"

nixCopyClosureTo :: (?sshConfig :: Maybe FilePath) => String -> FilePath -> Command Local
nixCopyClosureTo "localhost" path =
    Cmd Local (args ["ls", "-ld", "--", path]) "copyto"
nixCopyClosureTo host path =
    Cmd Local (args [ nixSshEnv
                    , "nix-copy-closure", "--gzip"
                    , "--to", host
                    , path
                    ]) $ mconcat [host, ":copyto"]

nixCopyClosureToI :: (?sshConfig :: Maybe FilePath) => Install -> Command Local
nixCopyClosureToI Install{ i_remote = (Remote _ host), i_storepath = path } =
    nixCopyClosureTo host path

nixCopyClosureFrom :: (?sshConfig :: Maybe FilePath) => String -> Install -> Command Remote
nixCopyClosureFrom from  Install{i_remote, i_storepath} =
    Cmd i_remote (args [ nixSshEnv
                       , "nix-copy-closure", "--gzip"
                       , "--from", from
                       , i_storepath
                       ])  "copyfrom"

nixSetProfileI :: Install -> Command Remote
nixSetProfileI Install{..} =
    forward i_remote $ nixSetProfile i_profile i_storepath

nixSetProfile :: FilePath -> StorePath -> Command Local
nixSetProfile i_profile i_storepath =
    Cmd Local (args ["nix-env", "-p", i_profile, "--set", i_storepath]) "set-profile"

nixSystemProfile :: FilePath
nixSystemProfile = "/nix/var/nix/profiles/system"

nixSwitchToConfiguration :: Install -> Command Remote
nixSwitchToConfiguration Install{i_remote = r@(Remote _ host)} =
    Cmd r (args [ env [("NIXOS_NO_SYNC", "1")]
                , mconcat [nixSystemProfile, "/bin/switch-to-configuration"]
                , "switch"]) "switch"

nixClosure :: FilePath -> Command Local
nixClosure path =
    Cmd Local (args ["nix-store", "-qR", path]) "closure"

nixTrySubstitutes :: String -> Install -> Command Remote
nixTrySubstitutes cache Install{i_remote = r@(Remote _ host), i_storepath} =
    Cmd r (args [ "nix-store", "-j4", "-r", "--ignore-unknown"
                , "--option", "use-ssh-substituter", "true"
                , "--option", "ssh-substituter-hosts", cache
                , i_storepath
                ]) "try-substitutes"

sshPrepKnownHost :: String -> Install -> Command Remote
sshPrepKnownHost known Install{i_remote = r@(Remote _ host)} =
    Cmd r (args [ "install", "-m", "700", "-d", "~/.ssh;"
                , "ssh-keygen", "-R", knownHost, ";"
                , "ssh-keyscan", "-t", "rsa,dsa", knownHost, ">", "~/.ssh/known_hosts"
                ]) "prep-cache-known-host"
  where
    knownHost = case split '@' $ C8.pack known of
                    [_, a] -> C8.unpack a
                    _ -> error "ssh closure cache must look like `user@hostname'."

ssh :: (?sshConfig :: Maybe FilePath) => Command Remote -> Command Local
ssh (Cmd (Remote _ "localhost") cmd desc) =
    Cmd Local cmd desc
ssh (Cmd (Remote _ host) cmd desc) =
    Cmd Local (args ["ssh", sshBaseOptions, host, "--", squote cmd]) $ mconcat [host, ":", desc]


forward :: Remote -> Command Local -> Command Remote
forward to (Cmd Local comm desc) = Cmd to comm desc
