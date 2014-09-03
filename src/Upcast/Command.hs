{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Upcast.Command (
  Local(..)
, Remote(..)
, Command(..)
, ExitCode(..)
, measure
, fgrun
, fgconsume
, spawn
, ssh
, sshBaseOptions
, sshMaster
, sshMasterExit
, sshFast
) where

import Control.Monad.Trans (liftIO)
import Control.Monad (ap, join, (<=<), when)
import System.FilePath (FilePath)
import System.Process (createProcess, waitForProcess, interruptProcessGroupOf,
                       CreateProcess(..), CmdSpec(..), StdStream(..), shell, ProcessHandle)
import System.Exit (ExitCode(..))
import GHC.IO.Handle (hClose, Handle, hSetBinaryMode)
import System.IO (hSetBuffering, BufferMode(..), stdout, stderr, IOMode(..), openFile, hPutStrLn)
import qualified System.IO as IO
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.IO (createPipe, fdToHandle)
import Control.Monad.Trans.Resource (runResourceT, liftResourceT, MonadResource(..))
import qualified Data.List as L
import Data.Maybe (maybe)
import Data.Monoid
import Data.Time.Clock
import Data.IORef

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

import Data.Conduit hiding (Flush(..))
import qualified Data.Conduit.List as CL

import Upcast.Interpolate (n)

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

fgrun :: Command Local -> IO ExitCode
fgrun c@(Cmd _ _ desc) = do
    print' Run c
    ref <- newIORef mempty
    (time, _) <- measure $ runResourceT $ run c $$ awaitForever $ liftIO . output ref
    code <- readIORef ref
    printExit Run code time
    return code
  where
    output ref (Flush code) = writeIORef ref code
    output ref (Chunk s) = B8.putStrLn $ mconcat [B8.pack $ applyColor 4 desc, "> ", s]

fgconsume :: Command Local -> IO BS.ByteString
fgconsume c@(Cmd Local s _) = do
    print' Consume c
    (time, (Just code, output)) <- measure $ (runResourceT $ proc $$ CL.consume) >>= return . concat
    printExit Consume code time
    return output
    where
      proc = bracketP (openFile "/dev/stderr" WriteMode) (hClose) $ \wh -> roProcessSource (cmd s) Nothing (Just wh)
      concat = L.foldl' mappend (Just ExitSuccess, BS.empty) . fmap chunk
      chunk (Chunk a) = (Just ExitSuccess, a)
      chunk (Flush code) = (Just code, BS.empty)

run :: MonadResource m => Command Local -> ProcessSource i m
run (Cmd Local s _) = roProcessSource (cmd s) Nothing Nothing

spawn :: Command Local -> IO ProcessHandle
spawn (Cmd Local s _) = do
    (_, _, _, handle) <- createProcess $ cmd s
    return handle

sshBaseOptions = [n|-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o PasswordAuthentication=no -o PreferredAuthentications=publickey -x|]

ssh :: Command Remote -> Command Local
ssh (Cmd (Remote (Just key) host) cmd desc) =
    Cmd Local [n|ssh ${sshBaseOptions} -i '#{key}' root@'#{host}' -- '#{cmd}'|] desc
ssh (Cmd (Remote Nothing host) cmd desc) =
    Cmd Local [n|ssh ${sshBaseOptions} root@'#{host}' -- '#{cmd}'|] desc

sshMaster controlPath (Remote key host) =
    Cmd Local [n|ssh -x root@'#{host}' -S '#{controlPath}' -M -N -f -oNumberOfPasswordPrompts=0 -oServerAliveInterval=60 -i '#{key}'|] host

sshMasterExit controlPath (Remote _ host) =
    Cmd Local [n|ssh root@'#{host}' -S '#{controlPath}' -O exit|] host

sshFast controlPath (Cmd (Remote key host) cmd desc) =
    Cmd Local [n|ssh -oControlPath='#{controlPath}' -i '#{key}' -x root@'#{host}' -- '#{cmd}'|] desc


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

mapMBoth :: Monad m => (t -> m a) -> (t, t) -> m (a, a)
mapMBoth f (a, b) = return (,) `ap` f a `ap` f b

mapMBoth_ :: Monad m => (t -> m a) -> (t, t) -> m ()
mapMBoth_ f (a, b) = f a >> f b  >> return ()

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

applyColor :: Int -> String -> String
applyColor index s = case needsColor of
                         True -> "\ESC[1;" ++ color ++ "m" ++ s ++ "\ESC[0m"
                         False -> s
  where
    color = show $ (31 + (index `mod` 7))

color Consume = 5
color Run = 3

needsColor = unsafePerformIO $ IO.hIsTerminalDevice stderr
output = IO.hPutStrLn stderr

printExit mode ExitSuccess time = output $ applyColor (color mode) $ mconcat ["completed in ", show time]
printExit mode (ExitFailure code) time = output $ applyColor 0 $ mconcat ["failed with ", show code, ", time: ", show time]

print' mode (Cmd Local comm desc) = output $ mconcat [applyColor 4 desc, "% ", applyColor (color mode) $ L.take 1000 comm]
