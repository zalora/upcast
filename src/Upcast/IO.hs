module Upcast.IO (
  module System.IO
, ASCIIColor(..)
, oops
, expect
, expectRight
, warn
, warn8
, applyColor
) where

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception

import Data.Monoid (mconcat)
import qualified Data.ByteString.Char8 as B8

data ASCIIColor = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
                deriving (Enum)

needsColor = unsafePerformIO $ hIsTerminalDevice stderr

applyColor :: ASCIIColor -> String -> String
applyColor color s = case needsColor of
                         True -> "\ESC[1;" ++ colorCode ++ "m" ++ s ++ "\ESC[0m"
                         False -> s
  where
    colorCode = show $ 30 + fromEnum color


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

warn = hPutStrLn stderr . mconcat
warn8 = B8.hPutStrLn stderr . mconcat
