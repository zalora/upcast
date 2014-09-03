{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards, NamedFieldPuns #-}

module Upcast.DeployCommands where

import Data.Text (Text)
import Data.ByteString.Char8 (intercalate)

import Upcast.Interpolate (n)

import Upcast.Nix
import Upcast.Types
import Upcast.Command

nixBaseOptions DeployContext{..} = [n| -I upcast=#{upcastNix} #{nixArgs} --show-trace |]

sshAgent socket = Cmd Local [n|ssh-agent -a #{socket}|] "agent"
sshAddKey socket key = Cmd Local [n|echo '#{key}' | env SSH_AUTH_SOCK=#{socket} SSH_ASKPASS=/usr/bin/true ssh-add -|] "agent"
sshAddKeyFile socket keyFile = Cmd Local [n|env SSH_AUTH_SOCK=#{socket} SSH_ASKPASS=/usr/bin/true ssh-add #{keyFile}|] "agent"
sshListKeys socket = Cmd Local [n|env SSH_AUTH_SOCK=#{socket} ssh-add -l|] "agent"

nixDeploymentInfo ctx expr uuid =
    Cmd Local [n|
      nix-instantiate #{nixBaseOptions ctx}
      --arg networkExprs '#{expr}'
      --argstr uuid #{uuid}
      '<upcast/eval-deployment.nix>'
      --eval-only --strict --read-write-mode
      -A info
    |] "info"

nixBuildMachines :: DeployContext -> String -> String -> String -> Command Local
nixBuildMachines ctx expr uuid outputPath =
    Cmd Local [n|
      nix-build #{nixBaseOptions ctx}
      --arg networkExprs '#{expr}'
      --argstr uuid #{uuid}
      '<upcast/eval-deployment.nix>'
      -A machines
      -o #{outputPath}
    |] "build"

nixCopyClosureTo sshAuthSock Install{ i_remote = (Remote _ host), i_closure = path } =
    Cmd Local [n|env SSH_AUTH_SOCK=#{sshAuthSock} NIX_SSHOPTS="#{sshBaseOptions}" nix-copy-closure --to root@#{host} #{path} --gzip|] host

nixCopyClosureToFast controlPath (Remote key host) path =
    Cmd Local [n|env NIX_SSHOPTS="-i #{key} -S #{controlPath} #{sshBaseOptions}" nix-copy-closure --to root@#{host} #{path} --gzip|] host

nixSetProfile :: Install -> Command Remote
nixSetProfile Install{i_closure, i_remote = r@(Remote _ host)} =
    Cmd r [n|nix-env -p /nix/var/nix/profiles/system --set "#{i_closure}"|] host

nixSwitchToConfiguration Install{i_remote = r@(Remote _ host)} =
    Cmd r [n|env NIXOS_NO_SYNC=1 /nix/var/nix/profiles/system/bin/switch-to-configuration switch|] host

nixClosure path =
    Cmd Local [n|nix-store -qR #{path}|] "closure"

nixTrySubstitutes Install{i_remote = r@(Remote _ host), i_paths} =
    Cmd r [n|nix-store -j 4 -r --ignore-unknown #{intercalate " " i_paths}|] host

ssh' :: Text -> Command Remote -> Command Local
ssh' sshAuthSock (Cmd (Remote _ host) cmd desc) =
    Cmd Local [n|env SSH_AUTH_SOCK=#{sshAuthSock} ssh #{sshBaseOptions} root@#{host} -- '#{cmd}'|] desc
