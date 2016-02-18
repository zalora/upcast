{-# LANGUAGE FlexibleInstances #-}

module Upcast.Infra.Input where

import Control.Applicative
import Control.Lens hiding (Context)
import Data.Aeson.Types (Value(..), ToJSON(..), FromJSON(..), Parser(..), parseMaybe, (.:), (.=))
import Data.Graph (vertices, graphFromEdges', topSort)
import Data.HashMap.Strict (elems)
import Data.Text (Text)
import Data.Witherable (Witherable(..))
import Upcast.Infra.Graph
import Upcast.Infra.Resource (Infra(..))
import Upcast.Infra.Types

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
