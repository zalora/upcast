{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards #-}

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

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

installMachines :: DeployContext -> [Machine] -> IO ()
installMachines ctx@DeployContext{envContext=e@EnvContext{..}, ..} machines = do
    installs <- mapM installP machines
    results <- mapConcurrently (try . go e) installs :: IO [Either SomeException ()]
    warn ["installs failed: ", show [i | (e, i) <- zip results installs, isLeft e]]
    return ()
  where
    resolveClosure :: Text -> IO StorePath
    resolveClosure hostname =
        case Map.lookup hostname closureSubstitutes of
            Just x -> return x
            _ -> readSymbolicLink $ closuresPath </> (T.unpack hostname)

    installP :: Machine -> IO Install
    installP Machine{..} = do
        i_closure <- resolveClosure m_hostname
        i_paths <- (fmap (split '\n') . fgconsume_ . nixClosure) $ i_closure
        let i_sshClosureCache = fmap (Remote Nothing) nixSSHClosureCache
            i_profile = nixSystemProfile
        return Install{..}
      where
        i_remote = Remote (T.unpack <$> m_keyFile) (T.unpack m_publicIp)
  
install :: InstallCli -> IO ()
install args@InstallCli{..} = do
  env <- readEnvContext
  let i_closure = ic_closure
      i_remote = Remote Nothing ic_target
      i_paths = []
      i_sshClosureCache = Remote Nothing <$> (nixSSHClosureCache env)
      i_profile = maybe nixSystemProfile id ic_profile
      i = Install{..}
  go env i

go :: EnvContext -> Install -> IO ()
go env install@Install{i_sshClosureCache = Just (Remote _ cacheHost)} = do
  fgssh $ sshPrepCacheKnownHost install
  go' env install
go env install = go' env install

go' :: EnvContext -> Install -> IO ()
go' EnvContext{..} install = do
  if (deployMode /= Unattended)
      then do
        fgssh . nixTrySubstitutes $ install
        fgrun' . nixCopyClosureTo sshAuthSock $ install
      else do
        fgssh . nixCopyClosureFrom $ install
  fgssh . nixSetProfile $ install
  when (i_profile install == nixSystemProfile) $ do
    fgssh . nixSwitchToConfiguration $ install

