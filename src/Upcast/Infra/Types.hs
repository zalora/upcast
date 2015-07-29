module Upcast.Infra.Types where

import Control.Applicative

import Data.Text (Text)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Upcast.Types
import Upcast.Infra.NixTypes

type Tags = [(Text, Text)]


type K = Text
type V = Text
type IDAlist = [(K, V)]

lookupOrId :: IDAlist -> InfraRef a -> Maybe V
lookupOrId alist (RefLocal x) = lookup x alist
lookupOrId alist (RefRemote x) = Just x

lookup_ :: IDAlist -> InfraRef a -> V
lookup_ alist ref = case lookupOrId alist ref of
                     Just x -> x
                     Nothing -> error $ "could not lookup resource for ref: " ++ show ref


forAttrs :: Applicative f => Map k v -> (k -> v -> f x) -> f [(k, x)]
forAttrs xs f = Map.toList <$> Map.traverseWithKey f xs
