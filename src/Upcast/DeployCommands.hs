{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards, NamedFieldPuns #-}

module Upcast.DeployCommands where

import Data.Text (Text)

import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Char8 (intercalate, split)

import Upcast.Interpolate (n)

import Upcast.Nix
import Upcast.Types
import Upcast.Command

nixBaseOptions :: DeployContext -> String
nixBaseOptions DeployContext{..} = [n| -I upcast=#{upcastNix} #{nixArgs} --show-trace |]

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

nixDeploymentInfo :: DeployContext -> String -> String -> Command Local
nixDeploymentInfo ctx expr uuid =
    Cmd Local [n|
      nix-instantiate #{nixBaseOptions ctx}
      --arg networkExprs '#{expr}'
      --argstr uuid #{uuid}
      '<upcast/eval-deployment.nix>'
      --eval-only --strict --read-write-mode
      -A info
    |] "info"

nixBuildMachines :: DeployContext -> String -> String -> Command Local
nixBuildMachines ctx@DeployContext{closuresPath} expr uuid =
    Cmd Local [n|
      nix-build #{nixBaseOptions ctx}
      --arg networkExprs '#{expr}'
      --argstr uuid #{uuid}
      '<upcast/eval-deployment.nix>'
      -A machines
      -o #{closuresPath}
    |] "build"

nixInstantiateMachines :: DeployContext -> String -> String -> Command Local
nixInstantiateMachines ctx expr uuid =
    Cmd Local [n|
      nix-instantiate #{nixBaseOptions ctx}
      --read-write-mode
      --argstr system x86_64-linux
      --arg networkExprs '#{expr}'
      --argstr uuid '#{uuid}'
      '<upcast/eval-deployment.nix>'
      -A remoteMachines
    |] "instantiate"

nixCopyClosureTo :: Show a => a -> Install -> Command Local
nixCopyClosureTo sshAuthSock Install{ i_remote = (Remote _ host), i_closure = path } =
    Cmd Local [n|env SSH_AUTH_SOCK=#{sshAuthSock} NIX_SSHOPTS="#{sshBaseOptions}" nix-copy-closure --to root@#{host} #{path} --gzip|] host

nixCopyClosureToFast :: (Show a, Show b) => a -> Remote -> b -> Command Local
nixCopyClosureToFast controlPath (Remote key host) path =
    Cmd Local [n|env NIX_SSHOPTS="-i #{key} -S #{controlPath} #{sshBaseOptions}" nix-copy-closure --to root@#{host} #{path} --gzip|] host

nixSetProfile :: Install -> Command Remote
nixSetProfile Install{i_closure, i_remote = r@(Remote _ host)} =
    Cmd r [n|nix-env -p /nix/var/nix/profiles/system --set "#{i_closure}"|] host

nixSwitchToConfiguration :: Install -> Command Remote
nixSwitchToConfiguration Install{i_remote = r@(Remote _ host)} =
    Cmd r [n|env NIXOS_NO_SYNC=1 /nix/var/nix/profiles/system/bin/switch-to-configuration switch|] host

nixClosure :: FilePath -> Command Local
nixClosure path =
    Cmd Local [n|nix-store -qR #{path}|] "closure"

nixTrySubstitutes :: Install -> Command Remote
nixTrySubstitutes Install{i_remote = r@(Remote _ host), i_sshClosureCache = Just (Remote _ cache), i_closure} =
    Cmd r [n|nix-store -j4 -r --ignore-unknown
             --option use-ssh-substituter true
             --option ssh-substituter-hosts #{cache} #{i_closure}|] host
nixTrySubstitutes Install{i_remote = r@(Remote _ host), i_paths} =
    Cmd r [n|nix-store -j4 -r --ignore-unknown #{intercalate " " i_paths}|] host

sshPrepCacheKnownHost :: Install -> Command Remote
sshPrepCacheKnownHost Install{i_remote = r@(Remote _ host), i_sshClosureCache = Just (Remote _ known)} =
    Cmd r [n|ssh-keygen -R #{knownHost}; ssh-keyscan -t rsa,dsa #{knownHost} > ~/.ssh/known_hosts|] host
  where
    [_, knownHost] = split '@' $ C8.pack known

ssh :: Text -> Command Remote -> Command Local
ssh sshAuthSock (Cmd (Remote _ host) cmd desc) =
    Cmd Local [n|env SSH_AUTH_SOCK=#{sshAuthSock} ssh #{sshBaseOptions} root@#{host} -- '#{cmd}'|] desc
