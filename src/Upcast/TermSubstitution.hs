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
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS

import Data.Aeson
import qualified Data.Aeson as A

type SubMap = Map Text Value
data SubStore = SubStore FilePath SubMap deriving (Show)
data Sub = Cached | Created deriving (Show)

loadSubStore :: FilePath -> IO SubStore
loadSubStore path = do
    Just store <- (A.decode :: LBS.ByteString -> Maybe SubMap) <$> LBS.readFile path
    return $ SubStore path store

commit :: SubStore -> IO SubStore
commit s@(SubStore path x) = do
    LBS.writeFile path $ A.encode x
    return s

-- XXX: Show is quite nice for debugging purposes (and readability, given thatkeys are expected to be small)
--      Perhaps pick another key serialization format (ToJSON won't help because terms can't be used as keys).
substitute :: (Show a) => SubStore -> a -> IO Value -> IO (Sub, SubStore, Value)
substitute state@(SubStore p map) term compute = maybe def wrap $ Map.lookup key map
  where
    key = T.pack $ show term
    wrap = return . (Cached, state, )
    def = do
      result <- compute
      state' <- commit $ SubStore p $ Map.insert key result map
      return (Created, state', result)
