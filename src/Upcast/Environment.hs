{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams #-}

module Upcast.Environment where

import           Control.Applicative
import           System.Directory (canonicalizePath)
import           System.Posix.Env (getEnv)
import           System.FilePath.Posix (replaceExtension)

import           Data.Aeson (eitherDecodeStrict, Value)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Maybe (catMaybes, fromMaybe, isJust, fromJust)
import           Data.Monoid (mempty)
import           Data.Text (Text)
import qualified Data.Text as T

import           Upcast.Deploy (nixCopyClosureTo, nixRealise, nixSetProfile)
import           Upcast.IO (expectRight, srsly)
import           Upcast.Infra.NixTypes (Infras)
import           Upcast.Monad (sequenceMaybe, when)
import           Upcast.Shell
import           Upcast.Types (StorePath, NixContext(..), InfraCli(..),
                               InfraContext(..), BuildRemote(..))

import           Paths_upcast (getDataFileName)

nixPath :: IO String
nixPath =
  fromJust <$> sequenceMaybe [ getEnv "NIX_UPCAST"
                              , Just <$> getDataFileName "nix"
                              , Just <$> return "nix" ]

nixContext :: FilePath -> IO NixContext
nixContext file = do
    nix_expressionFile <- canonicalizePath file
    upcastPath <- nixPath
    nixArgs <- getEnv "UPCAST_NIX_FLAGS"
    let nix_args = ["-I", "upcast=" <> upcastPath, "--show-trace"] <>
                   maybe mempty (fmap T.unpack . T.splitOn " " . T.pack) nixArgs
    nix_sshStoreCache <- getEnv "UPCAST_SSH_STORE_CACHE"
    return NixContext{..}

icontext :: InfraCli -> IO InfraContext
icontext infraCli@InfraCli{..} =
  nixContext infraCli_expressionFile >>= evalInfraContext infraCli

evalInfraContext :: InfraCli -> NixContext -> IO InfraContext
evalInfraContext InfraCli{..} nix@NixContext{nix_expressionFile=file} = do
  info <- fgconsume_ (nixInfraInfo nix)
  value <- expectRight $ return $ nixInfras info
  return InfraContext{ inc_expressionFile = file
                     , inc_stateFile = fromMaybe (replaceExtension file "store") infraCli_stateFile
                     , inc_infras = value
                     }

nixInfras :: ByteString -> Either String Infras
nixInfras = eitherDecodeStrict

nixInfraInfo :: NixContext -> Commandline
nixInfraInfo NixContext{..} =
  exec "nix-instantiate" (nix_args <>
                          [ "--argstr", "expr", nix_expressionFile
                          , "<upcast/eval-infra.nix>"
                          , "--eval-only", "--strict", "--json"
                          , "--read-write-mode"
                          ])

nixInstantiate :: [String] -> Maybe String -> String -> FilePath -> Commandline
nixInstantiate nix_args attr exprFile root =
  exec "nix-instantiate" (nix_args <>
                          [ "--read-write-mode"
                          , "--add-root", root
                          , "--indirect"
                          ] <> maybeKey "-A" attr <> [exprFile])


buildRemote :: BuildRemote -> IO FilePath
buildRemote BuildRemote{..} = nixContext brc_expressionFile >>= go
  where
    ssh_ = ssh brc_builder []
    fwd = fgrunDirect . ssh_
    copy = let ?sshConfig = Nothing in nixCopyClosureTo

    go :: NixContext -> IO FilePath
    go NixContext{..} = do
      drv <- fgtmp (nixInstantiate nix_args brc_attribute nix_expressionFile)

      srsly "nix-copy-closure failed" (fgrunDirect (copy brc_builder drv))
      srsly "realise failed" (fwd (nixRealise drv))
      out <- B8.unpack <$> fgconsume_ (ssh_ (exec "nix-store" ["-qu", drv]))
      when brc_cat $ do
        fwd (exec "cat" [toString out])
        return ()
      when (isJust brc_installProfile) $ do
        let Just prof = brc_installProfile
        fwd (nixSetProfile prof out)
        return ()
      return out
