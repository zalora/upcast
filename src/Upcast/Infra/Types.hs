{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Upcast.Infra.Types where

import           Control.Applicative
import           Control.Lens hiding ((.=))
import           Data.Aeson (object)
import           Data.Aeson.Types (Value(..), ToJSON(..), FromJSON(..), Parser(..), parseMaybe, (.:), (.=))
import           Data.Hashable (Hashable(..))
import           Data.List (partition)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Upcast.Infra.Graph
import           Upcast.Infra.NixTypes

type ResourceId = Text

-- *

data Reference = Reference Text Text deriving (Eq, Ord, Show, Generic)

instance Hashable Reference

instance ToJSON k => ToJSON (Map Reference k) where
  toJSON mr = object . Map.elems
            . Map.mapWithKey (\c m -> c .= object (map (uncurry (.=)) m))
            . flip (flip Map.foldrWithKey Map.empty) mr $ \(Reference c n) k tmp ->
                Map.insert c ((n, k) : Map.findWithDefault [] c tmp) tmp

data Missing = Missing { unMissing :: Reference }

lookup_ :: Map Reference ResourceId -> (Text -> Reference) -> InfraRef a -> Either Missing (InfraRef a)
lookup_ ledger report = \case
  RefLocal n -> case Map.lookup (report n) ledger of
    Just ref -> Right $ RefRemote ref
    Nothing  -> Left . Missing $ report n
  r@(RefRemote _) -> Right r

-- *

data MatchResult = OnwardWith ResourceId
                 | NeedsUpdate ResourceId

instance ToJSON MatchResult where
  toJSON (OnwardWith  id) = object ["OnwardWith" .= id]
  toJSON (NeedsUpdate id) = object ["NeedsUpdate" .= id]


data DiscoveryError = NotFound
                    | Ambiguous [Text]
                    deriving Show

toDiscovery :: [ResourceId] -> Either DiscoveryError ResourceId
toDiscovery = \case
  [one] -> Right one
  []    -> Left NotFound
  many  -> Left (Ambiguous many)

virtual :: Monad m => a -> m (Either DiscoveryError MatchResult)
virtual = const . return . Right $ NeedsUpdate "(virtual)"

-- *

fromRefRemote :: InfraRef a -> ResourceId
fromRefRemote (RefRemote r) = r

converge :: Eq a => [a] -> [a] -> ([a], [a])
converge from to = (add, remove)
  where (keep, remove) = partition (`elem` to) from
        add = filter (`notElem` keep) to

raise :: Functor f => Getter s a -> Getter (f s) (f a) -- XXX: Lawful?
raise l = to . fmap $ view l

append :: a -> Lens' [a] [a] -- XXX: Unlawful, be careful.
append a = \f s -> init <$> f (a:s)
