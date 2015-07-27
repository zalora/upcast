{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Upcast.Shell.Types (
  (<>) -- from Data.Semigroup
, Arg
, arg
, args
, render
, Expr
, Commandline
, (|:)
, (|>)
, exec
, env
, ssh
, sudo
, sh
, sha
, Hostname
, Str
, toString
, maybeKey
) where

import Data.Proxy
import GHC.TypeLits

import Control.Applicative
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Semigroup hiding (Arg)
import Data.String

type Str a = (IsString a, Show a)

toString :: Show a => a -> String
toString s = let s' = show s in fromMaybe s' (readMaybe s')

newtype Arg = Arg String deriving Show

instance IsString Arg where
  fromString "" = Arg "\"\""
  fromString x = Arg x

instance Semigroup Arg where
  (<>) = mappend

instance Monoid Arg where
  mempty = Arg ""

  (Arg "") `mappend` (Arg a) = Arg a
  (Arg a) `mappend` (Arg "") = Arg a
  (Arg a) `mappend` (Arg b) = Arg (a <> " " <> b)

render (Arg s) = s

env' :: Str a => [(String, a)] -> Arg
env' xs =  "env" <> mconcat (fmap (\(k, v) -> Arg (mconcat [k, "=", escape (toString v)])) xs)

type Hostname = String
type Executable = String
type Pair = (String, String)

data Expr :: * -> * where
  E     :: Executable -> e -> Expr e
  Pipe  :: Expr e -> Expr e -> Expr e
  Seq   :: Expr e -> Expr e -> Expr e
  Redir :: Expr e -> FilePath -> Expr e
  Env   :: [Pair] -> Expr e -> Expr e
  Sudo  :: Expr e -> Expr e
  SSH   :: Hostname -> e -> Expr e -> Expr e

instance Semigroup (Expr e) where
  a <> b = Seq a b

type Commandline = Expr [String]


exec = E
(|>) = Redir
(|:) = Pipe
sudo = Sudo
env = Env
ssh = SSH

sha :: Expr [String] -> Arg
sha (E exec args')   = arg exec <> args args'
sha (Pipe l r)       = sha l <> "|" <> sha r
sha (Seq l r)        = "{" <> sha l <> ";" <> sha r <> "; }"
sha (Env xs exp)     = env' xs <> sha exp
sha (Sudo exp)       = "sudo sh -c" <> arg (sh exp)
sha (Redir exp file) = "{" <> sha exp <> ">" <> arg file <> "; }"
sha (SSH host op exp)= "ssh" <> args op <> arg host <> arg (sh exp)

sh :: Expr [String] -> String
sh = render . sha

arg :: String -> Arg
arg = fromString . escape

args :: [String] -> Arg
args = foldr (\inc acc -> arg inc <> acc) mempty

escape :: String -> String
escape xs = if all safe xs then xs else escaped
  where
    escaped = "\"" ++ concatMap f xs ++ "\""

    f '\0' = ""
    f '"'  = "\\\""
    f '\\' = "\\\\"
    f '!'  = "\\!"
    f '$'  = "\\$"
    f '`'  = "\\`"
    f x    = [x]

    safe = (`elem` "-./0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_=")

maybeKey :: String -> Maybe String -> [String]
maybeKey k = maybe mempty (\v -> [k, v])
