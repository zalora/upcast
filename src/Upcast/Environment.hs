{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards #-}

module Upcast.Environment where

import System.Directory (canonicalizePath)
import System.Posix.Env (getEnvDefault, getEnv, setEnv)
import System.FilePath.Posix
import Options.Applicative

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Aeson (eitherDecodeStrict)

import Upcast.Monad
import Upcast.IO
import Upcast.Types
import Upcast.Temp
import Upcast.Command
import Upcast.DeployCommands (setupAgentF, sshAddKeyFile)

import Paths_upcast

sequenceMaybe :: Monad m => [m (Maybe a)] -> m (Maybe a)
sequenceMaybe [] = return Nothing
sequenceMaybe (act:actions) = act >>= maybe (sequenceMaybe actions) (return . Just)

nixPath :: IO (Maybe String)
nixPath = sequenceMaybe [getEnv "NIX_UPCAST", Just <$> getDataFileName "nix"]

nixContext :: FilePath -> IO NixContext
nixContext file = do
    nix_expressionFile <- canonicalizePath file
    Just upcastPath <- fmap T.pack <$> nixPath
    nixArgs <- T.pack <$> getEnvDefault "UPCAST_NIX_FLAGS" ""
    let nix_args = " -I upcast=" <> upcastPath <> " " <> nixArgs <> " --show-trace "

    nix_sshClosureCache <- getEnv "UPCAST_SSH_CLOSURE_CACHE"

    return NixContext{..}

parseSubstitutesMap :: String -> ReadM (Map Text StorePath)
parseSubstitutesMap s =
  case eitherDecodeStrict $ BS.pack s of
      Left e -> readerError e
      Right v -> return v

pullOption = optional (strOption
                        (long "pull"
                        <> short 'f'
                        <> metavar "FROM"
                        <> help "pull closures from host"))

runCli :: Parser RunCli
runCli = RunCli
    <$> optional (option (str >>= parseSubstitutesMap)
               (long "closures-map"
                <> short 'm'
                <> metavar "JSON OBJECT"
                <> help "json mapping from hostnames to nixos closures (see `instantiate' or `build')"))
    <*> pullOption
    <*> argument str (metavar "<expression>")

prepAuth :: [Text] -> IO ()
prepAuth keyFiles = do
    userAuthSock <- getEnv "UPCAST_SSH_AUTH_SOCK"
    agentSocket <- case userAuthSock of
                     Just sock -> do
                        warn ["Using UPCAST_SSH_AUTH_SOCK: ", sock]
                        return sock
                     Nothing | null keyFiles ->  fallback
                             | otherwise -> setupAgentF sshAddKeyFile keyFiles

    setEnv "SSH_AUTH_SOCK" agentSocket True
  where
    fallback = do
      sock <- getEnvDefault "SSH_AUTH_SOCK" ""
      warn [ "None of instances reference ssh key files, using SSH_AUTH_SOCK ("
           , show sock, ")."]
      when (null sock) $
        fail "SSH_AUTH_SOCK is not set, please setup your ssh agent with necessary keys."
      return sock

