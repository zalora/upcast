{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards, NamedFieldPuns #-}

module Upcast.DeployCommands where

import Data.Text (Text)
import Data.Monoid (mconcat)
import Data.String (IsString)

import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Char8 (intercalate, split)

import Upcast.Interpolate (n)

import Upcast.Nix
import Upcast.Types
import Upcast.Command
import Upcast.Temp (randomTempFileName)

sshBaseOptions :: String
sshBaseOptions = [n|-A -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o PasswordAuthentication=no -o PreferredAuthentications=publickey -x|]

sshAgent :: FilePath -> Command Local
sshAgent socket = Cmd Local [n|ssh-agent -a #{socket}|] "agent"

sshAddKey :: FilePath -> Text -> Command Local
sshAddKey socket key = Cmd Local [n|echo '#{key}' | env SSH_AUTH_SOCK=#{socket} SSH_ASKPASS=/usr/bin/true ssh-add -|] "agent"

sshAddKeyFile :: FilePath -> Text -> Command Local
sshAddKeyFile socket keyFile = Cmd Local [n|env SSH_AUTH_SOCK=#{socket} SSH_ASKPASS=/usr/bin/true ssh-add #{keyFile}|] "agent"

sshListKeys :: FilePath -> Command Local
sshListKeys socket = Cmd Local [n|env SSH_AUTH_SOCK=#{socket} ssh-add -l|] "agent"

setupAgent :: [Text] -> IO FilePath
setupAgent privkeys = setupAgentF sshAddKey privkeys

setupAgentF :: (FilePath -> a -> Command Local) -> [a] -> IO FilePath
setupAgentF liftKey keyvals = do
    agentSocket <- randomTempFileName "ssh-agent.sock."
    spawn $ sshAgent agentSocket
    mapM_ (fgrunDirect . liftKey agentSocket) $ keyvals
    fgrunDirect $ sshListKeys agentSocket
    return agentSocket

nixDeploymentInfo :: NixContext -> Command Local
nixDeploymentInfo NixContext{..} =
    Cmd Local [n|
      nix-instantiate #{nix_args}
      --argstr networkExprs '#{nix_expressionFile}'
      '<upcast/eval-deployment.nix>'
      --eval-only --strict --read-write-mode
      -A info
    |] "info"

nixBuildMachines :: NixContext -> Maybe FilePath -> Command Local
nixBuildMachines NixContext{..} closuresPath =
    Cmd Local [n|
      nix-build #{nix_args}
      --argstr networkExprs '#{nix_expressionFile}'
      '<upcast/eval-deployment.nix>' #{outLink}
    |] "build"
  where
    outLink = case closuresPath of
                  Nothing -> [n|-A remoteMachines --no-out-link|]
                  Just o -> [n|-A machines -o #{o}|]

nixInstantiateMachines :: NixContext -> FilePath -> Command Local
nixInstantiateMachines NixContext{..} root =
    Cmd Local [n|
      nix-instantiate #{nix_args}
      --show-trace
      --read-write-mode
      --argstr system x86_64-linux
      --argstr networkExprs '#{nix_expressionFile}'
      --add-root '#{root}'
      --indirect
      '<upcast/eval-deployment.nix>'
      -A remoteMachines
    |] "instantiate"

nixInstantiate :: (IsString a, Show a) =>
                  a -> Maybe String -> FilePath -> FilePath -> Command Local
nixInstantiate nix_args attr exprFile root =
    Cmd Local [n|
      nix-instantiate #{nix_args}
      --show-trace
      --read-write-mode
      --argstr system x86_64-linux
      --add-root '#{root}'
      --indirect #{attrString} '#{exprFile}'
    |] "instantiate"
  where
    attrString = case attr of
                    Nothing -> ""
                    Just a -> [n|-A '#{a}'|]

nixRealise :: FilePath -> Command Local
nixRealise drv = Cmd Local [n|nix-store --realise #{drv}|] "realise"

nixCopyClosureTo :: String -> FilePath -> Command Local
nixCopyClosureTo "localhost" path =
    Cmd Local [n|ls -ld -- #{path}|] "copyto"
nixCopyClosureTo host path =
    Cmd Local [n|env NIX_SSHOPTS="#{sshBaseOptions}" nix-copy-closure --to #{host} #{path} --gzip|] $ mconcat [host, ":copyto"]

nixCopyClosureToI :: Install -> Command Local
nixCopyClosureToI Install{ i_remote = (Remote _ host), i_closure = path } =
    nixCopyClosureTo host path

nixCopyClosureFrom :: String -> Install -> Command Remote
nixCopyClosureFrom from  Install{i_remote, i_closure} =
    Cmd i_remote [n|env NIX_SSHOPTS="#{sshBaseOptions}" nix-copy-closure --from #{from} #{i_closure} --gzip|] $ "copyfrom"

nixSetProfile :: Install -> Command Remote
nixSetProfile Install{i_closure, i_profile, i_remote = r@(Remote _ host)} =
    Cmd r [n|nix-env -p #{i_profile} --set "#{i_closure}"|] "set-profile"

nixSystemProfile :: FilePath
nixSystemProfile = "/nix/var/nix/profiles/system"

nixSwitchToConfiguration :: Install -> Command Remote
nixSwitchToConfiguration Install{i_remote = r@(Remote _ host)} =
    Cmd r [n|env NIXOS_NO_SYNC=1 #{nixSystemProfile}/bin/switch-to-configuration switch|] "switch"

nixClosure :: FilePath -> Command Local
nixClosure path =
    Cmd Local [n|nix-store -qR #{path}|] "closure"

nixTrySubstitutes :: String -> Install -> Command Remote
nixTrySubstitutes cache Install{i_remote = r@(Remote _ host), i_closure} =
    Cmd r [n|nix-store -j4 -r --ignore-unknown
             --option use-ssh-substituter true
             --option ssh-substituter-hosts #{cache} #{i_closure}|] "try-substitutes"

sshPrepKnownHost :: String -> Install -> Command Remote
sshPrepKnownHost known Install{i_remote = r@(Remote _ host)} =
    Cmd r [n|install -m 700 -d ~/.ssh; ssh-keygen -R #{knownHost}; ssh-keyscan -t rsa,dsa #{knownHost} > ~/.ssh/known_hosts|] "prep-cache-known-host"
  where
    knownHost = case split '@' $ C8.pack known of
                    [_, a] -> a
                    _ -> error "ssh closure cache must look like `user@hostname'."

ssh :: Command Remote -> Command Local
ssh (Cmd (Remote _ "localhost") cmd desc) =
    Cmd Local cmd desc
ssh (Cmd (Remote _ host) cmd desc) =
    Cmd Local [n|ssh #{sshBaseOptions} #{host} -- '#{cmd}'|] $ mconcat [host, ":", desc]

forward :: Remote -> Command Local -> Command Remote
forward to (Cmd Local comm desc) = Cmd to comm desc
