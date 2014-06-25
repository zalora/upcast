module Upcast.Temp (
  writeTempFile
, randomTempFileName
) where

import Control.Exception (bracket)
import Control.Monad (replicateM)

import System.IO
import System.Directory
import System.FilePath ((</>))
import System.Random (randomRIO)

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
