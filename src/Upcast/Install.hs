{-# LANGUAGE TemplateHaskell, OverloadedStrings, RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE ImplicitParams #-}

module Upcast.Install (
  install
) where

import Control.Exception.Base (SomeException, try)

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text (Text(..))

import Control.Concurrent.Async
import System.FilePath.Posix
import System.Posix.Files (readSymbolicLink)
import System.Posix.Env (getEnv)
import Data.ByteString.Char8 (split)

import Upcast.Monad
import Upcast.IO
import Upcast.Types
import Upcast.Command
import Upcast.DeployCommands
import Upcast.Environment

data FgCommands =
  FgCommands { fgrun' :: Command Local -> IO ()
             , fgssh :: Command Remote -> IO ()
             }

fgCommands fgrun = FgCommands{..}
  where
    fgrun' = expect ExitSuccess "install step failed" . fgrun
    fgssh = fgrun' . ssh

install :: (Command Local -> IO ExitCode) -> InstallCli -> IO ()
install fgrun args@InstallCli{..} = do
  let i_closure = ic_closure
      i_remote = Remote Nothing ic_target
      i_paths = []
      i_profile = maybe nixSystemProfile id ic_profile
  let ?sshConfig = ic_sshConfig
  go (fgCommands fgrun) (toDelivery ic_pullFrom) Install{..}

go :: (?sshConfig :: Maybe FilePath) => FgCommands -> DeliveryMode -> Install -> IO ()
go FgCommands{..} dm install@Install{i_paths} = do
  nixSSHClosureCache <- getEnv "UPCAST_SSH_CLOSURE_CACHE"
  case nixSSHClosureCache of
      Just cache -> do
        fgssh $ sshPrepKnownHost cache install
        unless (null i_paths) $ fgssh . nixTrySubstitutes cache $ install
      _ -> return ()

  case dm of
      Push -> fgrun' . nixCopyClosureToI $ install
      Pull from -> fgssh . nixCopyClosureFrom from $ install

  fgssh $ nixSetProfileI install
  when (i_profile install == nixSystemProfile) $ do
    fgssh . nixSwitchToConfiguration $ install

