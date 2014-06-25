{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Upcast.Command where

import Control.Monad.Trans (liftIO)
import Control.Monad (ap, join, (<=<))
import System.FilePath (FilePath)
import System.Process (createProcess, waitForProcess, CreateProcess(..), CmdSpec(..), StdStream(..))
import GHC.IO.Handle (hClose, Handle, hSetBinaryMode)
import System.IO (hSetBuffering, BufferMode(..), stdout, IOMode(..), openFile)
import System.Posix.IO (createPipe, fdToHandle)
import Data.Maybe (maybe)
import Control.Monad.Trans.Resource (runResourceT, liftResourceT, MonadResource(..))

import Data.ByteString as BS

import Data.Conduit
import qualified Data.Conduit.List as CL
import Blaze.ByteString.Builder

import Upcast.Interpolate (n)

type ChunkSource m = Source m (Flush Builder)
type Key = FilePath

data Local = Local deriving (Show)
data Remote = Remote Key String deriving (Show)

data Command a = Cmd a String deriving (Show)

fgrun c = run c $$ awaitForever $ liftIO . BS.putStr . chunk

fgconsume (Cmd Local s) = (runResourceT $ proc $$ CL.consume) >>= return . concat
    where
      proc = bracketP (openFile "/dev/null" WriteMode) hClose $ \null -> roProcessSource (cmd s) Nothing (Just null)
      concat = BS.concat . fmap chunk

chunk (Chunk a) = toByteString a
chunk _ = ""

run (Cmd Local s) = roProcessSource (cmd s) Nothing Nothing

ssh :: Command Remote -> Command Local
ssh (Cmd (Remote key host) cmd) =
    Cmd Local [n|ssh -i #{key} -x root@#{host} -- '#{cmd}'|]

sshMaster controlPath (Remote key host) =
    Cmd Local [n|ssh -x root@#{host} -S #{controlPath} -M -N -f -oNumberOfPasswordPrompts=0 -oServerAliveInterval=60 -i #{key}|]

sshMasterExit controlPath (Remote _ host) =
    Cmd Local [n|ssh root@#{host} -S #{controlPath} -O exit|]

sshFast controlPath (Cmd (Remote key host) cmd) =
    Cmd Local [n|ssh -oControlPath=#{controlPath} -i #{key} -x root@#{host} -- '#{cmd}'|]


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

roProcessSource proc stdout stderr =
  let
    stdout' d = maybe d id stdout
    stderr' d = maybe d id stderr
  in do
    handles@(rh, wh) <- liftIO createPipeHandle
    (Just stdin, _, _, _) <- liftIO $ createProcess proc { std_out = UseHandle $ stdout' wh
                                                         , std_err = UseHandle $ stderr' wh }
    liftIO $ hClose stdin
    bracketP (return handles) (mapMBoth_ hClose) (sourceHandle . fst)

mapMBoth :: Monad m => (t -> m a) -> (t, t) -> m (a, a)
mapMBoth f (a, b) = return (,) `ap` f a `ap` f b

mapMBoth_ f (a, b) = f a >> f b  >> return ()

createPipeHandle :: IO (Handle, Handle)
createPipeHandle = join $ fmap (mapMBoth fdToHandle) createPipe

sourceHandle h = loop
  where
    loop = do
        x <- liftIO $ BS.hGetSome h 128
        if BS.null x
            then return ()
            else do
              yield $ Chunk $ fromByteString x
              yield Flush
              loop

applyColor index s = "\ESC[1;" ++ color ++ "m" ++ s ++ "\ESC[0m"
  where
    color = show $ (31 + (index `mod` 7))

colorize = applyColor 5

