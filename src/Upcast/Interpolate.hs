{-# LANGUAGE TemplateHaskell #-}
module Upcast.Interpolate (n, nl) where

import Data.Char (isSpace)
import Data.List (dropWhile)

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.Meta.Parse.Careful (parseExp)
import Language.Haskell.TH (Q, Exp, appE, reportError)

import Upcast.Interpolate.Util
import Upcast.Interpolate.Parse

strip [] = []
strip ('\n':' ':a) = strip ('\n' : a)
strip ('\n':[]) = []
strip ('\n':a) = strip (' ' : a)
strip (a:b) = a : strip b

-- | Just like `Data.String.Interpolate.i` but drops leading whitespace.
n :: QuasiQuoter
n = QuasiQuoter {
    quoteExp = toExp . parseNodes . dropWhile isSpace
  , quotePat = err "pattern"
  , quoteType = err "type"
  , quoteDec = err "declaration"
  }
  where
    err name = error ("Upcast.Interpolate.n: This QuasiQuoter can not be used as a " ++ name ++ "!")

    toExp :: [Node] -> Q Exp
    toExp nodes = case nodes of
      [] -> [|""|]
      (x:xs) -> f x `appE` toExp xs
      where
        f (Literal s) = [|showString $ strip s|]
        f (Expression e) = [|(showString . toString) $(reifyExpression e)|]

        reifyExpression :: String -> Q Exp
        reifyExpression s = case parseExp s of
          Left _ -> do
            reportError "Parse error in expression!"
            [|""|]
          Right e -> return e

-- | newline-preserving strip
stripnl [] = []
stripnl ('\n':' ':a) = strip ('\n' : a)
stripnl ('\n':[]) = []
stripnl ('\n':a) = strip (' ' : a)
stripnl (a:b) = a : strip b

nl :: QuasiQuoter
nl = QuasiQuoter {
    quoteExp = toExp . parseNodes
  , quotePat = err "pattern"
  , quoteType = err "type"
  , quoteDec = err "declaration"
  }
  where
    err name = error ("Upcast.Interpolate.nl: This QuasiQuoter can not be used as a " ++ name ++ "!")

    toExp :: [Node] -> Q Exp
    toExp nodes = case nodes of
      [] -> [|""|]
      (x:xs) -> f x `appE` toExp xs
      where
        f (Literal s) = [|showString s|]
        f (Expression e) = [|(showString . toString) $(reifyExpression e)|]

        reifyExpression :: String -> Q Exp
        reifyExpression s = case parseExp s of
          Left _ -> do
            reportError "Parse error in expression!"
            [|""|]
          Right e -> return e

