module Upcast.IO (
  module System.IO
, ASCIIColor(..)
, applyColor
, oops
, expect
, srsly
, expectRight
, warn
, warn8
, pprint
) where

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (ExitCode(..))
import Control.Exception

import Data.Monoid (mconcat, (<>))
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LBS

import Data.Aeson.Types (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)

data ASCIIColor = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
                deriving (Enum)

needsColor = unsafePerformIO $ hIsTerminalDevice stderr

applyColor :: ASCIIColor -> String -> String
applyColor color s = case needsColor of
                         True -> "\ESC[1;" ++ colorCode ++ "m" ++ s ++ "\ESC[0m"
                         False -> s
  where
    colorCode = show $ 30 + fromEnum color


oops :: String -> IO a
oops = throwIO . ErrorCall

expect :: Eq a => a -> String -> IO a -> IO ()
expect value excuse action = do
  result <- action
  case result of
      x | x == value -> return ()
      _ -> oops excuse

srsly :: String -> IO ExitCode -> IO ()
srsly = expect ExitSuccess

expectRight :: IO (Either String a) -> IO a
expectRight action = do
  result <- action
  case result of
      Right smth -> return smth
      Left err -> oops err

warn :: [String] -> IO ()
warn = hPutStrLn stderr . mconcat

warn8 :: [B8.ByteString] -> IO ()
warn8 = B8.hPutStrLn stderr . mconcat

pprint :: ToJSON a => a -> IO ()
pprint = LBS.putStrLn . encodePretty
