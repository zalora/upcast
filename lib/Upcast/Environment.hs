{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams #-}

module Upcast.Environment where

import           Control.Applicative
import           Control.Lens ((<&>), (&))
import           System.Directory (canonicalizePath)
import           System.Posix.Env (getEnv)

import           Data.Aeson (eitherDecodeStrict)
import qualified Data.ByteString.Char8 as B8
import           Data.Maybe (isJust, fromJust)

import           Infracast.Input (InfraContext(..))
import           Upcast.Deploy (nixCopyClosureTo, nixRealise, nixSetProfile)
import           Upcast.IO (expectRight, srsly)
import           Upcast.Monad (sequenceMaybe, when)
import           Upcast.Shell
import           Upcast.Types (NixContext(..), InfraCli(..),
                               Build(..))

import           Paths_upcast (getDataFileName)

nixPath :: IO String
nixPath =
  fromJust <$> sequenceMaybe [ getEnv "NIX_UPCAST"
                              , Just <$> getDataFileName "nix"
                              , Just <$> return "nix" ]

nixContext :: FilePath -> [String] -> IO NixContext
nixContext file args = do
    nix_expressionFile <- canonicalizePath file
    upcastPath <- nixPath
    let nix_args = ["-I", "upcast=" <> upcastPath, "--show-trace"] <> args
    return NixContext{..}

icontext :: InfraCli -> IO InfraContext
icontext InfraCli{..} = nixContext infraCli_expressionFile infraCli_extra >>= \nix -> do
  infras <- fgconsume_ (nixInfraInfo nix) <&> eitherDecodeStrict & expectRight
  return $ InfraContext infraCli_expressionFile infras infraCli_verbose

nixInfraInfo :: NixContext -> Commandline
nixInfraInfo NixContext{..} =
  exec "nix-instantiate" (nix_args <>
                          [ "--argstr", "expr", nix_expressionFile
                          , "<upcast/eval-infra.nix>"
                          , "--eval-only", "--strict", "--json"
                          ])

nixInstantiate :: [String] -> Maybe String -> String -> FilePath -> Commandline
nixInstantiate nix_args attr exprFile root =
  exec "nix-instantiate" (nix_args <>
                          [ "--add-root", root
                          , "--indirect"
                          ] <> maybeKey "-A" attr <> [exprFile])


build :: Build -> IO FilePath
build Build{..} = nixContext b_expressionFile b_extra >>= go
  where
    ssh_ = case b_builder of
                "localhost" -> id
                _ -> ssh b_builder []
    fwd = fgrunDirect . ssh_
    copy = let ?sshConfig = Nothing in nixCopyClosureTo

    go :: NixContext -> IO FilePath
    go NixContext{..} = do
      drv <- fgtmp (nixInstantiate nix_args b_attribute nix_expressionFile)

      srsly "nix-copy-closure failed" (fgrunDirect (copy b_builder drv))
      srsly "realise failed" (fwd (nixRealise drv))
      out <- B8.unpack <$> fgconsume_ (ssh_ (exec "nix-store" ["-qu", drv]))
      when b_cat $ do
        fwd (exec "cat" [toString out])
        return ()
      when (isJust b_installProfile) $ do
        let Just prof = b_installProfile
        fwd (nixSetProfile prof out)
        return ()
      return out
