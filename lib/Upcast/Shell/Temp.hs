module Upcast.Shell.Temp (
  writeTempFile
, randomTempFileName
, fgtmp
) where

import Control.Exception (bracket)
import Control.Monad (replicateM)

import System.Directory (removeFile, getTemporaryDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Posix.Files (readSymbolicLink)
import System.Random (randomRIO)
import System.IO (openTempFile, hPutStrLn, hClose)

import Upcast.Shell.Run
import Upcast.Shell.Types
import Upcast.IO (expect)

writeTempFile :: FilePath -> String -> IO FilePath
writeTempFile name s = do
    tmp <- getTemporaryDirectory
    bracket (openTempFile tmp name)
            (\(_, h) -> hClose h)
            $ \(p, h) -> do
              hPutStrLn h s
              return p

randomTempFileName :: String -> IO FilePath
randomTempFileName prefix = do
    tmp <- getTemporaryDirectory
    filename <- replicateM 15 $ randomRIO ('a', 'z')
    return $ tmp </> prefix ++ filename

fgtmp :: (FilePath -> Commandline) -> IO FilePath
fgtmp f =
  bracket (randomTempFileName "fgtmp.") removeFile $ \tmp -> do
    expect ExitSuccess "failed" (fgrunDirect (f tmp))
    readSymbolicLink tmp
