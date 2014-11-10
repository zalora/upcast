{-# LANGUAGE TemplateHaskell, OverloadedStrings, RecordWildCards, NamedFieldPuns #-}

module Upcast.Install (
  installMachines
, install
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

fgrun' :: Command Local -> IO ()
fgrun' = expect ExitSuccess "install step failed" . fgrun

fgssh :: Command Remote -> IO ()
fgssh = fgrun' . ssh

installMachines :: DeliveryMode -> (Hostname -> IO StorePath) -> [Machine] -> IO ()
installMachines dm resolveClosure machines = do
    installs <- mapM installP machines
    results <- mapConcurrently (try . go dm) installs :: IO [Either SomeException ()]
    warn ["installs failed: ", show [i | (e, i) <- zip results installs, isLeft e]]
    return ()
  where
    isLeft :: Either a b -> Bool
    isLeft (Left _) = True
    isLeft _ = False

    installP :: Machine -> IO Install
    installP Machine{..} = do
        nixSSHClosureCache <- getEnv "UPCAST_SSH_CLOSURE_CACHE"
        i_closure <- resolveClosure m_hostname
        i_paths <- (fmap (split '\n') . fgconsume_ . nixClosure) $ i_closure
        return Install{..}
      where
        i_remote = Remote (T.unpack <$> m_keyFile) (T.unpack m_publicIp)
        i_profile = nixSystemProfile
  
install :: InstallCli -> IO ()
install args@InstallCli{..} = do
  let i_closure = ic_closure
      i_remote = Remote Nothing ic_target
      i_paths = []
      i_profile = maybe nixSystemProfile id ic_profile
  go (toDelivery ic_pullFrom) Install{..}

go :: DeliveryMode -> Install -> IO ()
go dm install@Install{i_paths} = do
  nixSSHClosureCache <- getEnv "UPCAST_SSH_CLOSURE_CACHE"
  case nixSSHClosureCache of
      Just cache -> do
        fgssh $ sshPrepKnownHost cache install
        unless (null i_paths) $ fgssh . nixTrySubstitutes cache $ install
      _ -> return ()

  case dm of
      Push -> fgrun' . nixCopyClosureToI $ install
      Pull from -> fgssh . nixCopyClosureFrom from $ install

  fgssh . nixSetProfile $ install
  when (i_profile install == nixSystemProfile) $ do
    fgssh . nixSwitchToConfiguration $ install

