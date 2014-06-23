module Upcast.Interpolate.Util where

import           Data.Char
import           Data.Maybe
import qualified Numeric as N

import           Text.Read

toString :: Show a => a -> String
toString a = let s = show a in fromMaybe s (readMaybe s)
{-# RULES "toString/String" toString = id #-}
{-# RULES "toString/Int" toString = show :: Int -> String #-}
{-# RULES "toString/Integer" toString = show :: Integer -> String #-}
{-# RULES "toString/Float" toString = show :: Float -> String #-}
{-# RULES "toString/Double" toString = show :: Double -> String #-}

-- Haskell 2010 character unescaping, see:
-- http://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-200002.6
unescape :: String -> String
unescape = go
  where
    go input = case input of
      "" -> ""
      '\\' : 'x' : x : xs | isHexDigit x -> case span isHexDigit xs of
        (ys, zs) -> (chr . readHex $ x:ys) : go zs
      '\\' : 'o' : x : xs | isOctDigit x -> case span isOctDigit xs of
        (ys, zs) -> (chr . readOct $ x:ys) : go zs
      '\\' : x : xs | isDigit x -> case span isDigit xs of
        (ys, zs) -> (chr . read $ x:ys) : go zs
      '\\' : input_ -> case input_ of
        'a' : xs -> '\a' : go xs
        'b' : xs -> '\b' : go xs
        'f' : xs -> '\f' : go xs
        'n' : xs -> '\n' : go xs
        'r' : xs -> '\r' : go xs
        't' : xs -> '\t' : go xs
        'v' : xs -> '\v' : go xs
        '&' : xs -> go xs
        'N':'U':'L' : xs -> '\NUL' : go xs
        'S':'O':'H' : xs -> '\SOH' : go xs
        'S':'T':'X' : xs -> '\STX' : go xs
        'E':'T':'X' : xs -> '\ETX' : go xs
        'E':'O':'T' : xs -> '\EOT' : go xs
        'E':'N':'Q' : xs -> '\ENQ' : go xs
        'A':'C':'K' : xs -> '\ACK' : go xs
        'B':'E':'L' : xs -> '\BEL' : go xs
        'B':'S' : xs -> '\BS' : go xs
        'H':'T' : xs -> '\HT' : go xs
        'L':'F' : xs -> '\LF' : go xs
        'V':'T' : xs -> '\VT' : go xs
        'F':'F' : xs -> '\FF' : go xs
        'C':'R' : xs -> '\CR' : go xs
        'S':'O' : xs -> '\SO' : go xs
        'S':'I' : xs -> '\SI' : go xs
        'D':'L':'E' : xs -> '\DLE' : go xs
        'D':'C':'1' : xs -> '\DC1' : go xs
        'D':'C':'2' : xs -> '\DC2' : go xs
        'D':'C':'3' : xs -> '\DC3' : go xs
        'D':'C':'4' : xs -> '\DC4' : go xs
        'N':'A':'K' : xs -> '\NAK' : go xs
        'S':'Y':'N' : xs -> '\SYN' : go xs
        'E':'T':'B' : xs -> '\ETB' : go xs
        'C':'A':'N' : xs -> '\CAN' : go xs
        'E':'M' : xs -> '\EM' : go xs
        'S':'U':'B' : xs -> '\SUB' : go xs
        'E':'S':'C' : xs -> '\ESC' : go xs
        'F':'S' : xs -> '\FS' : go xs
        'G':'S' : xs -> '\GS' : go xs
        'R':'S' : xs -> '\RS' : go xs
        'U':'S' : xs -> '\US' : go xs
        'S':'P' : xs -> '\SP' : go xs
        'D':'E':'L' : xs -> '\DEL' : go xs
        '^':'@' : xs -> '\^@' : go xs
        '^':'A' : xs -> '\^A' : go xs
        '^':'B' : xs -> '\^B' : go xs
        '^':'C' : xs -> '\^C' : go xs
        '^':'D' : xs -> '\^D' : go xs
        '^':'E' : xs -> '\^E' : go xs
        '^':'F' : xs -> '\^F' : go xs
        '^':'G' : xs -> '\^G' : go xs
        '^':'H' : xs -> '\^H' : go xs
        '^':'I' : xs -> '\^I' : go xs
        '^':'J' : xs -> '\^J' : go xs
        '^':'K' : xs -> '\^K' : go xs
        '^':'L' : xs -> '\^L' : go xs
        '^':'M' : xs -> '\^M' : go xs
        '^':'N' : xs -> '\^N' : go xs
        '^':'O' : xs -> '\^O' : go xs
        '^':'P' : xs -> '\^P' : go xs
        '^':'Q' : xs -> '\^Q' : go xs
        '^':'R' : xs -> '\^R' : go xs
        '^':'S' : xs -> '\^S' : go xs
        '^':'T' : xs -> '\^T' : go xs
        '^':'U' : xs -> '\^U' : go xs
        '^':'V' : xs -> '\^V' : go xs
        '^':'W' : xs -> '\^W' : go xs
        '^':'X' : xs -> '\^X' : go xs
        '^':'Y' : xs -> '\^Y' : go xs
        '^':'Z' : xs -> '\^Z' : go xs
        '^':'[' : xs -> '\^[' : go xs
        '^':'\\' : xs -> '\^\' : go xs
        '^':']' : xs -> '\^]' : go xs
        '^':'^' : xs -> '\^^' : go xs
        '^':'_' : xs -> '\^_' : go xs
        xs -> go xs
      x:xs -> x : go xs

    readHex :: String -> Int
    readHex xs = case N.readHex xs of
      [(n, "")] -> n
      _ -> error "Upcast.Interpolate.Util.readHex: no parse"

    readOct :: String -> Int
    readOct xs = case N.readOct xs of
      [(n, "")] -> n
      _ -> error "Upcast.Interpolate.Util.readHex: no parse"
