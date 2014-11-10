{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards, NamedFieldPuns #-}

module Upcast.DeployCommands where

import Data.Text (Text)
import Data.Monoid (mconcat)

import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Char8 (intercalate, split)

import Upcast.Interpolate (n)

import Upcast.Nix
import Upcast.Types
import Upcast.Command
import Upcast.Temp (randomTempFileName)

nixBaseOptions :: EnvContext -> String
nixBaseOptions EnvContext{..} = [n| -I upcast=#{upcastNix} #{nixArgs} --show-trace |]

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
    mapM_ (fgrun . liftKey agentSocket) $ keyvals
    fgrun $ sshListKeys agentSocket
    return agentSocket

nixDeploymentInfo :: DeployContext -> String -> String -> Command Local
nixDeploymentInfo DeployContext{envContext} expr uuid =
    Cmd Local [n|
      nix-instantiate #{nixBaseOptions envContext}
      --arg networkExprs '#{expr}'
      --argstr uuid #{uuid}
      '<upcast/eval-deployment.nix>'
      --eval-only --strict --read-write-mode
      -A info
    |] "info"

nixBuildMachines :: DeployContext -> String -> String -> Command Local
nixBuildMachines DeployContext{closuresPath, envContext} expr uuid =
    Cmd Local [n|
      nix-build #{nixBaseOptions envContext}
      --arg networkExprs '#{expr}'
      --argstr uuid #{uuid}
      '<upcast/eval-deployment.nix>'
      -A machines
      -o #{closuresPath}
    |] "build"

nixInstantiateMachines :: DeployContext -> String -> String -> String -> Command Local
nixInstantiateMachines DeployContext{envContext} expr uuid root =
    Cmd Local [n|
      nix-instantiate #{nixBaseOptions envContext}
      --read-write-mode
      --argstr system x86_64-linux
      --arg networkExprs '#{expr}'
      --argstr uuid '#{uuid}'
      --add-root '#{root}'
      --indirect
      '<upcast/eval-deployment.nix>'
      -A remoteMachines
    |] "instantiate"

nixCopyClosureTo :: Show a => a -> Install -> Command Local
nixCopyClosureTo sshAuthSock Install{ i_remote = (Remote _ host), i_closure = path } =
    Cmd Local [n|env SSH_AUTH_SOCK=#{sshAuthSock} NIX_SSHOPTS="#{sshBaseOptions}" nix-copy-closure --to root@#{host} #{path} --gzip|] $ mconcat [host, ":copyto"]

nixCopyClosureFrom :: Install -> Command Remote
nixCopyClosureFrom Install{i_remote, i_closure, i_sshClosureCache=Just (Remote _ host)} =
    Cmd i_remote [n|env NIX_SSHOPTS="#{sshBaseOptions}" nix-copy-closure --from #{host} #{i_closure} --gzip|] $ "copyfrom"

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

nixTrySubstitutes :: Install -> Command Remote
nixTrySubstitutes Install{i_remote = r@(Remote _ host), i_sshClosureCache = Just (Remote _ cache), i_closure} =
    Cmd r [n|nix-store -j4 -r --ignore-unknown
             --option use-ssh-substituter true
             --option ssh-substituter-hosts #{cache} #{i_closure}|] "try-substitutes"
nixTrySubstitutes Install{i_remote = r@(Remote _ host), i_paths} =
    Cmd r [n|nix-store -j4 -r --ignore-unknown #{intercalate " " i_paths}|] "try-substitutes"

sshPrepCacheKnownHost :: Install -> Command Remote
sshPrepCacheKnownHost Install{i_remote = r@(Remote _ host), i_sshClosureCache = Just (Remote _ known)} =
    Cmd r [n|ssh-keygen -R #{knownHost}; ssh-keyscan -t rsa,dsa #{knownHost} > ~/.ssh/known_hosts|] "prep-cache-known-host"
  where
    knownHost = case split '@' $ C8.pack known of
                    [_, a] -> a
                    _ -> error "ssh closure cache must look like `user@hostname'."

sshA :: Text -> Command Remote -> Command Local
sshA sshAuthSock (Cmd (Remote _ host) cmd desc) =
    Cmd Local [n|env SSH_AUTH_SOCK=#{sshAuthSock} ssh #{sshBaseOptions} root@#{host} -- '#{cmd}'|] $ mconcat [host, ":", desc]

ssh :: Command Remote -> Command Local
ssh (Cmd (Remote _ host) cmd desc) =
    Cmd Local [n|ssh #{sshBaseOptions} root@#{host} -- '#{cmd}'|] $ mconcat [host, ":", desc]
