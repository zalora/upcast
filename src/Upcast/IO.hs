module Upcast.IO (
  module System.IO
, expect
, expectRight
, warn
) where

import System.IO
import Control.Exception

oops = throwIO . ErrorCall

expect :: Eq a => a -> String -> IO a -> IO ()
expect value excuse action = do
  result <- action
  case result of
      x | x == value -> return ()
      _ -> oops excuse

expectRight :: IO (Either String a) -> IO a
expectRight action = do
  result <- action
  case result of
      Right smth -> return smth
      Left err -> oops err

warn = hPutStrLn stderr . concat

