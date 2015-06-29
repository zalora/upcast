{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Upcast.Install (
  install
) where

import System.Posix.Env (getEnv)
import Data.List (break)

import Upcast.Deploy (nixSetProfile, ssh, sshBaseOptions, nixSshEnv,
                      nixCopyClosureTo, nixSystemProfile)
import Upcast.IO (expect)
import Upcast.Monad (whenJustM, when, unless)
import Upcast.Shell (Commandline, ExitCode(..), fgrunDirect, (|:),
                     exec, (|>), (<>), env, toArg, render)
import Upcast.Types (Remote(..), Install(..), DeliveryMode(..))

fgrun = expect ExitSuccess "install step failed" . fgrunDirect

install :: Install -> IO ()
install install@Install{..} = do
  let ?sshConfig = i_sshConfig

  whenJustM (getEnv "UPCAST_SSH_STORE_CACHE") $ \cache ->
    fgrun (prepKnownHostI cache install)

  case i_delivery of
   Push -> fgrun (nixCopyClosureToI install)
   Pull "localhost" -> return ()
   Pull from -> fgrun (nixCopyClosureFromI from install)

  fgrun (nixSetProfileI install)
  when (i_profile == nixSystemProfile) $ fgrun (nixSwitchToConfiguration install)

forward :: (?sshConfig :: Maybe FilePath) => Remote -> Commandline -> Commandline
forward (Remote host) = ssh host

nixSetProfileI :: (?sshConfig :: Maybe FilePath) => Install -> Commandline
nixSetProfileI Install{..} =
    forward i_remote (nixSetProfile i_profile i_storepath)

nixSwitchToConfiguration :: (?sshConfig :: Maybe FilePath) => Install -> Commandline
nixSwitchToConfiguration Install{i_remote = r@(Remote host)} =
  ssh host (env [("NIXOS_NO_SYNC", "1")]
            (exec (nixSystemProfile <> "/bin/switch-to-configuration") ["switch"]))

prepKnownHostI cache Install{i_remote} = forward i_remote (prepKnownHost cache)

nixCopyClosureToI :: (?sshConfig :: Maybe FilePath) => Install -> Commandline
nixCopyClosureToI Install{i_remote=(Remote host), i_storepath} =
  nixCopyClosureTo host i_storepath

nixCopyClosureFromI :: (?sshConfig :: Maybe FilePath) => String -> Install -> Commandline
nixCopyClosureFromI from Install{i_remote, i_storepath} =
  forward i_remote (nixSshEnv (exec "nix-copy-closure" ["--gzip", "--from", from, i_storepath]))

testClosureCache cache =
  case break (=='@') cache of
   (_:_, '@':_) -> cache
   _ -> error "ssh closure cache target must look like `user@hostname'"

prepKnownHost :: String -> Commandline
prepKnownHost (testClosureCache -> knownHost) =
  exec "install" ["-m", "700", "-d", "~/.ssh"] <>
  exec "ssh-keygen" ["-R", knownHost] <>
  exec "ssh-keyscan" ["-t", "rsa,dsa", knownHost] |> "~/.ssh/known_hosts"
