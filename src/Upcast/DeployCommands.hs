{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards #-}

module Upcast.DeployCommands where

import Data.Text (Text)
import Data.ByteString.Char8 (intercalate)

import Upcast.Interpolate (n)

import Upcast.Nix
import Upcast.Types
import Upcast.Command

nixBaseOptions DeployContext{..} = [n| -I upcast=#{upcastNix} #{nixArgs} --show-trace |]

sshAgent socket = Cmd Local [n|ssh-agent -a #{socket}|]
sshAddKey socket key = Cmd Local [n|echo '#{key}' | env SSH_AUTH_SOCK=#{socket} SSH_ASKPASS=/usr/bin/true ssh-add -|]
sshAddKeyFile socket keyFile = Cmd Local [n|env SSH_AUTH_SOCK=#{socket} SSH_ASKPASS=/usr/bin/true ssh-add #{keyFile}|]
sshListKeys socket = Cmd Local [n|env SSH_AUTH_SOCK=#{socket} ssh-add -l|]

nixCopyClosureTo sshAuthSock (Remote _ host) path =
    Cmd Local [n|env SSH_AUTH_SOCK=#{sshAuthSock} NIX_SSHOPTS="#{sshBaseOptions}" nix-copy-closure --to root@#{host} #{path} --gzip|]

nixCopyClosureToFast controlPath (Remote key host) path =
    Cmd Local [n|env NIX_SSHOPTS="-i #{key} -S #{controlPath} #{sshBaseOptions}" nix-copy-closure --to root@#{host} #{path} --gzip|]

nixDeploymentInfo ctx expr uuid = Cmd Local [n|
                     nix-instantiate #{nixBaseOptions ctx}
                     --arg networkExprs '#{expr}'
                     --arg args {}
                     --argstr uuid #{uuid}
                     '<upcast/eval-deployment.nix>'
                     --eval-only --strict --read-write-mode
                     --arg checkConfigurationOptions false
                     -A info
                     |]

nixBuildMachines :: DeployContext -> String -> String -> [String] -> String -> Command Local
nixBuildMachines ctx expr uuid names outputPath = Cmd Local [n|
                   env NIX_BUILD_HOOK="$HOME/.nix-profile/libexec/nix/build-remote.pl"
                   NIX_REMOTE_SYSTEMS="$HOME/remote-systems.conf"
                   NIX_CURRENT_LOAD="/tmp/load2"
                   TEST=1
                   nix-build #{nixBaseOptions ctx}
                   --arg networkExprs '#{expr}'
                   --arg args {}
                   --argstr uuid #{uuid}
                   '<upcast/eval-deployment.nix>'
                   -A machines
                   -o #{outputPath}
                   |]

nixSetProfile remote closure = Cmd remote [n|
                                  nix-env -p /nix/var/nix/profiles/system --set "#{closure}"
                                  |]

nixSwitchToConfiguration remote = Cmd remote [n|
                                  env NIXOS_NO_SYNC=1 /nix/var/nix/profiles/system/bin/switch-to-configuration switch
                                  |]

nixClosure path = Cmd Local [n|nix-store -qR #{path}|]
nixTrySubstitutes remote closure = Cmd remote [n|nix-store -j 4 -r --ignore-unknown #{intercalate " " closure}|]

ssh' :: Text -> Command Remote -> Command Local
ssh' sshAuthSock (Cmd (Remote _ host) cmd) =
    Cmd Local [n|env SSH_AUTH_SOCK=#{sshAuthSock} ssh #{sshBaseOptions} root@#{host} -- '#{cmd}'|]
