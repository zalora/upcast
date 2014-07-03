{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Upcast.Command where

import Control.Monad.Trans (liftIO)
import Control.Monad (ap, join, (<=<))
import System.FilePath (FilePath)
import System.Process (createProcess, waitForProcess, CreateProcess(..), CmdSpec(..), StdStream(..), shell, ProcessHandle)
import System.Exit (ExitCode(..))
import GHC.IO.Handle (hClose, Handle, hSetBinaryMode)
import System.IO (hSetBuffering, BufferMode(..), stdout, IOMode(..), openFile)
import System.Posix.IO (createPipe, fdToHandle)
import Control.Monad.Trans.Resource (runResourceT, liftResourceT, MonadResource(..))
import Data.List as L
import Data.Maybe (maybe)
import Data.Monoid

import Data.ByteString as BS

import Data.Conduit
import qualified Data.Conduit.List as CL
import Blaze.ByteString.Builder

import Upcast.Interpolate (n)

type ProcessSource i m = ConduitM i ((Maybe ExitCode, Flush Builder)) m ()
type Key = FilePath

data Local = Local deriving (Show)
data Remote = Remote Key String deriving (Show)

data Command a = Cmd a String deriving (Show)

instance Monoid ExitCode where
    mempty = ExitSuccess
    _ `mappend` e@(ExitFailure _) = e
    e@(ExitFailure _) `mappend` _ = e
    a `mappend` b = a


fgrun :: Command Local -> IO ()
fgrun c = do
    printC c
    runResourceT $ run c $$ awaitForever $ liftIO . output . chunk
  where
    output (maybeCode, s) = do
      BS.putStr s
      case maybeCode of
        Just c -> printC c
        Nothing -> return ()

fgconsume :: Command Local -> IO BS.ByteString
fgconsume c@(Cmd Local s) = do
    printC c
    (Just code, output) <- (runResourceT $ proc $$ CL.consume) >>= return . concat
    printC code
    return output
    where
      proc = bracketP (openFile "/dev/stderr" WriteMode) (hClose) $ \wh -> roProcessSource (cmd s) Nothing (Just wh)
      concat = L.foldl' mappend (Just ExitSuccess, BS.empty) . fmap chunk

chunk (x, Chunk a) = (x, toByteString a)
chunk (x, _) = (x, BS.empty)

run :: MonadResource m => Command Local -> ProcessSource i m
run (Cmd Local s) = roProcessSource (cmd s) Nothing Nothing

spawn :: Command Local -> IO ProcessHandle
spawn (Cmd Local s) = do
    (_, _, _, handle) <- createProcess $ cmd s
    return handle

ssh :: Command Remote -> Command Local
ssh (Cmd (Remote key host) cmd) =
    Cmd Local [n|ssh -i '#{key}' -x root@'#{host}' -- '#{cmd}'|]

sshMaster controlPath (Remote key host) =
    Cmd Local [n|ssh -x root@'#{host}' -S '#{controlPath}' -M -N -f -oNumberOfPasswordPrompts=0 -oServerAliveInterval=60 -i '#{key}'|]

sshMasterExit controlPath (Remote _ host) =
    Cmd Local [n|ssh root@'#{host}' -S '#{controlPath}' -O exit|]

sshFast controlPath (Cmd (Remote key host) cmd) =
    Cmd Local [n|ssh -oControlPath='#{controlPath}' -i '#{key}' -x root@'#{host}' -- '#{cmd}'|]


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
    bracketP (return handles) (mapMBoth_ hClose) (\(rh, _) -> sourceHandle (Just prochn) rh)

mapMBoth :: Monad m => (t -> m a) -> (t, t) -> m (a, a)
mapMBoth f (a, b) = return (,) `ap` f a `ap` f b

mapMBoth_ :: Monad m => (t -> m a) -> (t, t) -> m ()
mapMBoth_ f (a, b) = f a >> f b  >> return ()

createPipeHandle :: IO (Handle, Handle)
createPipeHandle = join $ fmap (mapMBoth fdToHandle) createPipe

sourceHandle :: MonadResource m => Maybe ProcessHandle -> Handle -> ProcessSource i m
sourceHandle ph h = loop
  where
    loop = do
        x <- liftIO $ BS.hGetSome h 128
        if BS.null x
            then 
            case ph of
              Just handle -> do
                code <- liftIO $ waitForProcess handle
                yield (Just code, Flush)
              Nothing -> return ()
            else do
              yield $ (Nothing, Chunk $ fromByteString x)
              yield (Nothing, Flush)
              loop

applyColor :: Int -> String -> String
applyColor index s = "\ESC[1;" ++ color ++ "m" ++ s ++ "\ESC[0m"
  where
    color = show $ (31 + (index `mod` 7))

colorize :: String -> String
colorize = applyColor 5

printC c = Prelude.putStrLn $ colorize $ show c
