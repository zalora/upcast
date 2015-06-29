{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Upcast.TermSubstitution (
  SubStore
, Sub(..)
, loadSubStore
, emptyStore
, substitute
) where

import System.Posix.Files (fileExist)
import Control.Applicative
import Data.Monoid (mconcat)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Text (Text)
import qualified Data.ByteString.Lazy as LBS

import Data.Aeson (Value)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A

type SubMap = Map Text Value
data SubStore = SubStore FilePath SubMap deriving (Show)
data Sub = Cached | Created deriving (Show)

emptyStore :: SubStore
emptyStore = SubStore "" (Map.empty :: SubMap)

loadSubStore :: FilePath -> IO SubStore
loadSubStore path = do
    exists <- fileExist path
    case exists of
      True -> return ()
      False -> LBS.writeFile path "{}"
    store <- (A.eitherDecode :: LBS.ByteString -> Either String SubMap) <$> LBS.readFile path
    case store of
      Right s -> return $ SubStore path s
      Left r -> error $ mconcat ["loadSubStore ", show path, ": ", r]

commit :: SubStore -> IO SubStore
commit s@(SubStore "" _) = return s
commit s@(SubStore path x) = do
    LBS.writeFile path $ A.encodePretty x
    return s

substitute :: SubStore -> Text -> IO Value -> IO (Sub, SubStore, Value)
substitute state@(SubStore p map) key compute = maybe def wrap $ Map.lookup key map
  where
    wrap = return . (Cached, state, )
    def = do
      result <- compute
      state' <- commit $ SubStore p $ Map.insert key result map
      return (Created, state', result)
