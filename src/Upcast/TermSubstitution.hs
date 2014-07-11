{-# LANGUAGE OverloadedStrings
           , TupleSections
           #-}

module Upcast.TermSubstitution (
  SubStore
, Sub(..)
, loadSubStore
, substitute
) where

import Control.Applicative

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

loadSubStore :: FilePath -> IO SubStore
loadSubStore path = do
    Just store <- (A.decode :: LBS.ByteString -> Maybe SubMap) <$> LBS.readFile path
    return $ SubStore path store

commit :: SubStore -> IO SubStore
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
