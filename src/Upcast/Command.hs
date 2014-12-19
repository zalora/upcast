{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Upcast.Command (
  Local(..)
, Remote(..)
, Command(..)
, ExitCode(..)
, measure
, fgrunProxy
, fgrunPipe
, fgrunPty
, fgrunDirect
, fgconsume
, fgconsume_
, spawn
) where

import Upcast.Monad
import qualified Upcast.IO as IO
import Upcast.IO (openFile, warn, warn8, applyColor)

import Control.Monad.Trans.Resource

import System.FilePath (FilePath)
import System.Process (createProcess, waitForProcess, interruptProcessGroupOf,
                       CreateProcess(..), CmdSpec(..), StdStream(..), shell,
                       ProcessHandle)
import System.Exit (ExitCode(..))
import GHC.IO.Handle (hClose, Handle)
import System.IO.Error (tryIOError)
import System.Posix.IO (createPipe, fdToHandle)
import System.Posix.Pty (spawnWithPty, readPty, Pty)
import System.Posix.Env (getEnv)
import qualified Data.List as L
import Data.Monoid
import Data.Time.Clock
import Data.IORef

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

import Data.Conduit hiding (Flush(..))
import qualified Data.Conduit.List as CL

data Flush a = Chunk a | Flush ExitCode
type ProcessSource i m = ConduitM i (Flush ByteString) m ()
type Key = FilePath

data Local = Local deriving (Show)
data Remote = Remote (Maybe Key) String deriving (Show)

data Command a = Cmd a String String deriving (Show)
data Mode = Consume | Run

instance Monoid ExitCode where
    mempty = ExitSuccess
    _ `mappend` e@(ExitFailure _) = e
    e@(ExitFailure _) `mappend` _ = e
    a `mappend` b = a

measure :: IO a -> IO (NominalDiffTime, a)
measure action = do
    now <- getCurrentTime
    result <- action
    then' <- getCurrentTime
    return (diffUTCTime then' now, result)

fgrunProxy :: Command Local -> IO ExitCode
fgrunProxy cmd = do
  mPty <- getEnv "UPCAST_NO_PTY"
  let f = maybe runPty (const run) mPty
  fgrun1 f cmd

fgrunPipe :: Command Local -> IO ExitCode
fgrunPipe = fgrun1 run

fgrunPty :: Command Local -> IO ExitCode
fgrunPty = fgrun1 runPty

fgrun1 :: (Command Local -> ProcessSource () (ResourceT IO)) -> Command Local -> IO ExitCode
fgrun1 runner c@(Cmd _ _ desc) = do
    print' Run c
    ref <- newIORef mempty
    (time, _) <- measure $ runResourceT $ runner c $$ awaitForever $ liftIO . output ref
    code <- readIORef ref
    printExit Run c code time
    return code
  where
    output ref (Flush code) = writeIORef ref code
    output ref (Chunk s) = warn8 [ (B8.pack $ applyColor IO.Magenta desc)
                                 , "> ", s ]

fgrunDirect :: Command Local -> IO ExitCode
fgrunDirect c@(Cmd Local comm _) = do
    print' Run c
    (_, _, _, phandle) <- createProcess cproc
    (time, code) <- measure $ waitForProcess phandle
    printExit Run c code time
    return code
  where
    cproc = CreateProcess { cmdspec = ShellCommand comm
                          , cwd = Nothing
                          , env = Nothing
                          , std_in = Inherit
                          , std_out = Inherit
                          , std_err = Inherit
                          , close_fds = True
                          , create_group = False
                          , delegate_ctlc = True
                          }

fgconsume :: Command Local -> IO (Either BS.ByteString BS.ByteString)
fgconsume c@(Cmd Local s _) = do
    print' Consume c
    (time, (Just code, output)) <- measure $ (runResourceT $ proc $$ CL.consume) >>= return . concat
    printExit Consume c code time
    case code of
        ExitSuccess -> return $ Right output
        _ -> return $ Left output
    where
      proc = bracketP (openFile "/dev/stderr" IO.WriteMode) (hClose) $ \wh -> roProcessSource (cmd s) Nothing (Just wh)
      concat = L.foldl' mappend (Just ExitSuccess, BS.empty) . fmap chunk
      chunk (Chunk a) = (Just ExitSuccess, a)
      chunk (Flush code) = (Just code, BS.empty)

fgconsume_ :: Command Local -> IO BS.ByteString
fgconsume_ c = either id id <$> fgconsume c

run :: MonadResource m => Command Local -> ProcessSource i m
run (Cmd Local s _) = roProcessSource (cmd s) Nothing Nothing

runPty :: MonadResource m => Command Local -> ProcessSource i m
runPty (Cmd Local s _) = do
  (pty, proc) <- liftIO $ spawnWithPty Nothing True "/bin/sh" ["-c", s] (80, 25)
  sourcePty (Just proc) pty

spawn :: Command Local -> IO ProcessHandle
spawn (Cmd Local s _) = do
    (_, _, _, handle) <- createProcess $ cmd s
    return handle

sourcePty :: MonadResource m => Maybe ProcessHandle -> Pty -> ProcessSource i m
sourcePty proc pty = loop ""
  where
    onEof = case proc of
              Just handle -> do
                code <- liftIO $ waitForProcess handle
                yield (Flush code)
              Nothing -> return ()

    push (x:[]) = return x
    push (x:y:[]) | y == B8.empty = yield (Chunk x) >> return ""
    push (x:xs) = yield (Chunk x) >> push xs
    push [] = return ""

    leftover "" = return ()
    leftover x = yield (Chunk x)

    loop acc = do
      err <- liftIO $ (tryIOError . readPty) pty
      case err of
         Left _ -> leftover acc >> onEof
         Right s -> do
           let x:xs = B8.split '\n' s
           yield (Chunk $ mconcat [acc, x])
           push xs >>= loop

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

roProcessSource
  :: MonadResource m =>
       CreateProcess
            -> Maybe Handle -> Maybe Handle -> ProcessSource i m
roProcessSource proc stdout stderr =
  let
    stdout' d = maybe d id stdout
    stderr' d = maybe d id stderr
  in do
    handles@(rh, wh) <- liftIO createPipeHandle
    (Just stdin, _, _, prochn) <- liftIO $ createProcess proc { std_out = UseHandle $ stdout' wh
                                                              , std_err = UseHandle $ stderr' wh }
    liftIO $ hClose stdin
    bracketP (return handles)
             (\hs -> mapMBoth_ hClose hs >> interruptProcessGroupOf prochn)
             (\(rh, _) -> sourceHandle (Just prochn) rh)

createPipeHandle :: IO (Handle, Handle)
createPipeHandle = join $ fmap (mapMBoth fdToHandle) createPipe

sourceHandle :: MonadResource m => Maybe ProcessHandle -> Handle -> ProcessSource i m
sourceHandle ph h = loop
  where
    onEof = case ph of
              Just handle -> do
                code <- liftIO $ waitForProcess handle
                yield (Flush code)
              Nothing -> return ()
    loop = do
        eof <- liftIO $ IO.hIsEOF h
        case eof of
          True -> onEof
          False -> do
            x <- liftIO $ BS.hGetLine h
            if BS.null x
              then onEof
              else yield (Chunk x) >> loop


color Consume = IO.Cyan
color Run = IO.Blue

prefix (Cmd Local _ desc) = mconcat [applyColor IO.Magenta desc, "% "]

printExit mode cmd ex time = warn [ prefix cmd, suffix ex ]
  where
    suffix (ExitFailure code) = applyColor IO.Red $ "failed with "
                                                  <> show code
                                                  <> ", time: "
                                                  <> show time
    suffix ExitSuccess = applyColor (color mode) $ "completed in " <> show time

print' mode c@(Cmd Local comm desc) =
  warn [prefix c, applyColor (color mode) $ L.take 1000 comm]
