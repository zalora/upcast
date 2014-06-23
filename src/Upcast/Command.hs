{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Upcast.Command where

import Control.Monad.Trans (liftIO)
import Control.Monad (ap, join)
import System.FilePath (FilePath)
import System.Process (createProcess, waitForProcess, CreateProcess(..), CmdSpec(..), StdStream(..))
import GHC.IO.Handle (hClose, Handle)
import System.IO (hSetBuffering, BufferMode(..))
import System.Posix.IO (createPipe, fdToHandle)

import Data.ByteString as BS

import Data.Conduit
import Blaze.ByteString.Builder

import Upcast.Interpolate (n)

type ChunkSource = Source IO (Flush Builder)
type Key = FilePath

data Local = Local deriving (Show)
data Remote = Remote Key String deriving (Show)

data Command a = Cmd a String deriving (Show)


run :: Command Local -> IO ChunkSource
run (Cmd Local s) = go s
  where
    go = fmap sourceHandle . runCreateProcess . cmd

ssh :: Command Remote -> Command Local
ssh (Cmd (Remote key host) cmd) =
    Cmd Local [n|ssh -i #{key} -x root@#{host} -- '#{cmd}'|]


cmd :: String -> CreateProcess
cmd s = CreateProcess { cmdspec = ShellCommand s
                      , cwd = Nothing
                      , env = Nothing
                      , std_in = CreatePipe
                      , std_out = CreatePipe
                      , std_err = CreatePipe
                      , close_fds = True
                      , create_group = True
                      , delegate_ctlc = False
                      }

runCreateProcess :: CreateProcess -> IO Handle
runCreateProcess proc = do
    (rh, wh) <- createPipeHandle
    hSetBuffering rh LineBuffering
    hSetBuffering wh LineBuffering
    (Just stdin, _, _, _) <- createProcess proc{std_out = UseHandle wh, std_err = UseHandle wh}
    hClose stdin
    return rh

mapMBoth :: Monad m => (t -> m a) -> (t, t) -> m (a, a)
mapMBoth f (a, b) = return (,) `ap` f a `ap` f b

createPipeHandle :: IO (Handle, Handle)
createPipeHandle = join $ fmap (mapMBoth fdToHandle) createPipe

sourceHandle :: Handle -> ChunkSource
sourceHandle h = do
    loop
  where
    loop = do
        x <- liftIO $ BS.hGetSome h 64
        if BS.null x
            then return ()
            else do
              yield $ Chunk $ fromByteString x
              yield Flush
              loop
