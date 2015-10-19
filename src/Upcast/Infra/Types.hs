{-# LANGUAGE FlexibleInstances #-}

module Upcast.Infra.Types
( module Upcast.Infra.Types.Common
, module Upcast.Infra.Types.Amazonka
, module Upcast.Infra.Types.Graph
, module Upcast.Infra.Types.Infra
, module Upcast.Infra.Types.Resource
, Infras(..)
) where

import Upcast.Infra.Types.Common
import Upcast.Infra.Types.Amazonka
import Upcast.Infra.Types.Graph
import Upcast.Infra.Types.Infra
import Upcast.Infra.Types.Resource

import Control.Applicative ((<$>),(<*>),pure,empty)
import Control.Lens -- (*)
import Data.Aeson (Value(..), FromJSON(..), (.:), object)
import Data.Aeson.Types (Parser(..), parseMaybe)
import Data.Graph (vertices, graphFromEdges', topSort)
import Data.HashMap.Strict (elems)
import Data.Text (Text)

data Infras = Infras
  { realmName :: Text
  , regions :: [Text]
  , resources :: Graph Reference Infra
  }

parseReference :: Value -> Parser Reference
parseReference (Object h) = Reference <$> h .: "_type" <*> h .: "local"
parseReference _ = empty

parseVertex :: Value -> Parser (Infra, Reference, [Reference])
parseVertex o@(Object h) = (,,)
  <$> parseJSON o
  <*> (Reference <$> h .: "_type" <*> h .: "_name") -- Top-level / non-canonical.
  <*> (concatMap (deep (to (parseMaybe parseReference) . _Just) & toListOf)
  <$> pure (elems h))
parseVertex _ = empty

instance FromJSON (Graph Reference Infra) where
  parseJSON o@(Object _) = pure . Graph
                         . (\(g,h) -> (g, \v -> if v `elem` vertices g then Just (h v) else Nothing))
                         . graphFromEdges'
                         $ o ^.. deep (to (parseMaybe parseVertex) . _Just)
  parseJSON _ = empty

instance FromJSON Infras where
  parseJSON obj@(Object o) = Infras
                         <$> o .: "realm-name"
                         <*> o .: "regions"
                         <*> parseJSON obj
  parseJSON _ = empty
