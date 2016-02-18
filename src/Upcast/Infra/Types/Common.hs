{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Upcast.Infra.Types.Common
( AWS
, Reference(..)
, Context(..)
, State(..)
, Tags
-- *
, MatchResult(..)
, DiscoveryError(..)
, toDiscovery
, Missing(..)
, hashOf
, defer
, virtual
, lookup_
, converge
, fromRefRemote
, isTag
, hashTagIs
, _filter
, raise
, append
, toEc2Tags
, gimme
) where

import Control.Applicative
import Control.Lens hiding (Context)
import Control.Monad (void, (<=<))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.AWS (Env, Rs, AWSPager(..), send, await, paginate, AWST, runAWST)
import Control.Monad.Trans.Resource
import Data.Conduit (runConduit, tryC, fuse)
import Data.Conduit.List (consume)
import Data.Hashable (Hashable(..))
import Data.Map (Map, toList, insert, elems)
import qualified Data.Map as Map (lookup)
import Data.Monoid (Monoid(..), (<>))
import Data.List (partition)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Network.AWS.Types (Error)
import qualified Network.AWS.EC2 as EC2
import Network.AWS.Env (HasEnv(..))
import Upcast.Infra.NixTypes -- (*)
import Upcast.Infra.Types.Amazonka (ResourceId)

type AWS m = ( Functor m
             , Applicative m
             , MonadCatch m
             , MonadThrow m
             , MonadResource m
             , MonadBaseControl IO m
             , MonadReader Context m
             , MonadState State m
             )

type Tags = [(Text, Text)]

data Reference = Reference Text Text deriving (Eq, Ord, Show, Generic)

instance Hashable Reference

data Context = Context
  { ctxEnv  :: Env
  , ctxTags :: Tags
  }

instance HasEnv Context where
  environment f ctx = f (ctxEnv ctx) <&> \env -> ctx { ctxEnv = env }

data State = State
  { stateKeyPairs  :: [Ec2keypair]
  , stateInstances :: [EC2.Instance]
  }

instance Monoid State where
  mempty = State [] []
  State ks0 is0 `mappend` State ks1 is1 = State (ks0 `mappend` ks1) (is0 `mappend` is1)

-- *

data MatchResult = OnwardWith ResourceId
                 | NeedsUpdate ResourceId

data DiscoveryError = NotFound | Ambiguous [Text] deriving Show

toDiscovery :: [Text] -> Either DiscoveryError Text
toDiscovery = \case
  [one] -> Right one
  []    -> Left NotFound
  many  -> Left (Ambiguous many)


data Missing = Missing { unMissing :: Reference }

hashOf :: Hashable h => h -> Text
hashOf = pack . show . hash

defer :: AWS m => AWST (ResourceT IO) a -> m ReleaseKey
defer act = asks ctxEnv >>= liftResourceT . register . void . runResourceT . flip runAWST act

virtual :: AWS m => a -> m (Either DiscoveryError MatchResult)
virtual = const . return . Right $ NeedsUpdate "(virtual)"

-- *


lookup_ :: Map Reference ResourceId -> (Text -> Reference) -> InfraRef a -> Either Missing (InfraRef a)
lookup_ ledger report = \case
  RefLocal n -> case Map.lookup (report n) ledger of
    Just ref -> Right $ RefRemote ref
    Nothing  -> Left . Missing $ report n
  r@(RefRemote _) -> Right r


fromRefRemote :: InfraRef a -> ResourceId
fromRefRemote (RefRemote r) = r

converge :: Eq a => [a] -> [a] -> ([a], [a])
converge from to = (add, remove)
  where (keep, remove) = partition (`elem` to) from
        add = filter (`notElem` keep) to


isTag :: Text -> Text -> (EC2.Tag -> Bool)
isTag key value tag = (tag ^. EC2.tagKey) == key && (tag ^. EC2.tagValue) == value

hashTagIs :: Hashable h => h -> (EC2.Tag -> Bool)
hashTagIs = isTag "hash" . hashOf -- # yolo

_filter :: Getter (Text, Text) EC2.Filter
_filter = \f t@(k, v) -> const t <$> f (EC2.filter' ("tag:" <> k) & EC2.fValues .~ [v])

raise :: Functor f => Getter s a -> Getter (f s) (f a) -- XXX: Lawful?
raise l = to . fmap $ view l

append :: a -> Lens' [a] [a] -- XXX: Unlawful, be careful.
append a = \f s -> init <$> f (a:s)

toEc2Tags xs tags =
  void . send $ EC2.createTags & EC2.cResources .~ xs
                               & EC2.cTags .~ map (uncurry EC2.tag) tags

gimme :: (AWS m, AWSPager rq) => rq -> m [Rs rq]
gimme = either (throwM :: MonadThrow m => Error -> m a) return
    <=< runConduit . tryC . flip fuse consume . paginate
