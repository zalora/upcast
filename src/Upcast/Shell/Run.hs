{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Upcast.Shell.Run (
  ExitCode(..)
, measure
, fgrunProxy
, fgrunPipe
, fgrunPty
, fgrunDirect
, fgconsume
, fgconsume_
, qconsume
, qconsume_
, spawn
) where

import           Upcast.Monad
import qualified Upcast.IO as IO
import           Upcast.IO (openFile, warn, warn8, applyColor)

import           Control.Monad.Trans.Resource

import           System.FilePath (FilePath)
import            System.Process (createProcess, waitForProcess, interruptProcessGroupOf,
                                  CreateProcess(..), CmdSpec(..), StdStream(..), shell,
                                  ProcessHandle)
import           System.Exit (ExitCode(..))
import           GHC.IO.Handle (hClose, Handle)
import           System.IO.Error (tryIOError)
import           System.Posix.IO (createPipe, fdToHandle)
import           System.Posix.Pty (spawnWithPty, readPty, Pty)
import           System.Posix.Env (getEnv)
import qualified Data.List as L
import           Data.Monoid
import           Data.Time.Clock
import           Data.IORef

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

import           Data.Conduit hiding (Flush(..))
import qualified Data.Conduit.List as CL

import           Upcast.Shell.Types (Commandline, sh)

data Flush a = Chunk a | Flush ExitCode
type ProcessSource i m = ConduitM i (Flush ByteString) m ()

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

fgrunProxy :: Commandline -> IO ExitCode
fgrunProxy cmd = do
  mPty <- getEnv "UPCAST_NO_PTY"
  let f = maybe runPty (const run) mPty
  fgrun1 f cmd

fgrunPipe :: Commandline -> IO ExitCode
fgrunPipe = fgrun1 run

fgrunPty :: Commandline -> IO ExitCode
fgrunPty = fgrun1 runPty

fgrun1 :: (Commandline -> ProcessSource () (ResourceT IO)) -> Commandline -> IO ExitCode
fgrun1 runner c = do
    print' Run c
    ref <- newIORef mempty
    (time, _) <- measure $ runResourceT $ runner c $$ awaitForever $ liftIO . output ref
    code <- readIORef ref
    printExit Run c code time
    return code
  where
    desc = ""
    output ref (Flush code) = writeIORef ref code
    output ref (Chunk s) = warn8 [ B8.pack $ applyColor IO.Magenta desc
                                 , "> ", s ]

fgrunDirect :: Commandline -> IO ExitCode
fgrunDirect c = do
    print' Run c
    (_, _, _, phandle) <- createProcess (interactiveProcess (sh c))
    (time, code) <- measure $ waitForProcess phandle
    printExit Run c code time
    return code

qconsume :: Commandline -> IO (Either BS.ByteString BS.ByteString)
qconsume comm = do
    (time, (Just code, output)) <- measure $ liftM concat (runResourceT $ proc $$ CL.consume)
    case code of
        ExitSuccess -> return $ Right output
        _ -> return $ Left output
    where
      proc = roProcessSource (pipeProcess (sh comm)) Nothing (Just IO.stderr)
      concat = L.foldl' mappend (Just ExitSuccess, BS.empty) . fmap chunk
      chunk (Chunk a) = (Just ExitSuccess, a)
      chunk (Flush code) = (Just code, BS.empty)

qconsume_ :: Commandline -> IO BS.ByteString
qconsume_ c = either id id <$> qconsume c


fgconsume :: Commandline -> IO (Either BS.ByteString BS.ByteString)
fgconsume comm = do
    print' Consume comm
    (time, (Just code, output)) <- measure $ liftM concat (runResourceT $ proc $$ CL.consume)
    printExit Consume comm code time
    case code of
        ExitSuccess -> return $ Right output
        _ -> return $ Left output
    where
      proc = roProcessSource (pipeProcess (sh comm)) Nothing (Just IO.stderr)
      concat = L.foldl' mappend (Just ExitSuccess, BS.empty) . fmap chunk
      chunk (Chunk a) = (Just ExitSuccess, a)
      chunk (Flush code) = (Just code, BS.empty)

fgconsume_ :: Commandline -> IO BS.ByteString
fgconsume_ c = either id id <$> fgconsume c

run :: MonadResource m => Commandline -> ProcessSource i m
run (sh -> c) = roProcessSource (pipeProcess c) Nothing Nothing

runPty :: MonadResource m => Commandline -> ProcessSource i m
runPty (sh -> c) = do
  (pty, proc) <- liftIO $ spawnWithPty Nothing True "/bin/sh" ["-c", c] (80, 25)
  sourcePty (Just proc) pty

spawn :: Commandline -> IO ProcessHandle
spawn (sh -> c) = do
    (_, _, _, handle) <- createProcess (pipeProcess c)
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

pipeProcess :: String -> CreateProcess
pipeProcess s = CreateProcess { cmdspec = ShellCommand s
                              , cwd = Nothing
                              , env = Nothing
                              , std_in = CreatePipe
                              , std_out = CreatePipe
                              , std_err = CreatePipe
                              , close_fds = True
                              , create_group = True
                              , delegate_ctlc = False
                              }

interactiveProcess :: String -> CreateProcess
interactiveProcess s = CreateProcess { cmdspec = ShellCommand s
                                     , cwd = Nothing
                                     , env = Nothing
                                     , std_in = Inherit
                                     , std_out = UseHandle IO.stderr
                                     , std_err = UseHandle IO.stderr
                                     , close_fds = True
                                     , create_group = False
                                     , delegate_ctlc = True
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

prefix = const "% "
--prefix desc = mconcat [applyColor IO.Magenta desc, "% "]

printExit mode cmd ex time = warn [ prefix cmd, suffix ex ]
  where
    suffix (ExitFailure code) = applyColor IO.Red $ "failed with "
                                                  <> show code
                                                  <> ", time: "
                                                  <> show time
    suffix ExitSuccess = applyColor (color mode) $ "completed in " <> show time

print' mode (sh -> comm) =
  warn [prefix comm, applyColor (color mode) $ L.take 1000 comm]
