{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ViewPatterns        #-}

module Upcast.Infra
( scan
, accum
, dump
) where

import Control.Applicative (Applicative, (<$>), empty)
import Control.Lens hiding (Context, (.=))
import Control.Monad.State (StateT(..))
import Control.Monad.Trans.AWS (envLogger, newLogger, LogLevel(Trace), newEnv, Credentials(Discover), runAWST, AWST')
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Foldable (foldrM)
import Data.Graph (topSort)
import Data.Map (Map, insert, foldrWithKey, mapWithKey, findWithDefault, elems)
import Data.Monoid ((<>), mempty)
import qualified Data.Map as Map (empty)
import Data.Text (Text, pack)
import Data.Witherable (catMaybes, mapMaybe)
import System.FilePath.Posix (splitFileName)
import System.IO (stderr)
import Upcast.Types (InfraContext(..), Machine)
import Upcast.Infra.Machine (machines)
import Upcast.Infra.Types (AWS(..), Graph(..), Infra, Resource(..), Reference(..), Context(..), State(..), MatchResult(..), Missing(..), DiscoveryError(..), ResourceId, validateRegion, Infras(..), splice)

-- Searches existing resources maching infra in the provided resource
-- graph, and yields what it finds.
scan :: InfraContext -> IO (Map Reference (Either Text MatchResult))
scan  = flip eval evalMatch

-- Realises the infra defined by the provided resource graph, and yields
-- descriptions of any built EC2 instances.
accum :: InfraContext -> IO [Machine]
accum = flip eval $ (>> machines) . evalAccum

-- Run some action on every node in the resource graph.
dump :: Applicative f => (Reference -> Infra -> f a) -> InfraContext -> f (Graph Reference a)
dump f = traverse (uncurry f) . splice . resources . inc_infras

evalAccum :: AWS m => Graph Reference Infra -> m (Map Reference ResourceId)
evalAccum (Graph (graph,resolve)) = topSort graph <&> resolve & foldrM evalAccum' Map.empty . catMaybes

evalAccum' :: AWS m => (Infra, Reference, [Reference]) -> Map Reference ResourceId -> m (Map Reference ResourceId)
evalAccum' (n,l,_) ledger = do
  let infra = either (\(unMissing -> x) -> error $ "Integrity error, missing " ++ show x) id $ reify ledger n
  match infra >>= \case
    Left  (NotFound)       -> create infra <&> flip (insert l) ledger
    Left  (Ambiguous as)   -> error $ "Ambiguous results, requires intervention: " ++ show (l, as)
    Right (OnwardWith id)  -> return $ insert l id ledger
    Right (NeedsUpdate id) -> update id infra <&> flip (insert l) ledger

evalMatch :: AWS m => Graph Reference Infra -> m (Map Reference (Either Text MatchResult))
evalMatch (Graph (graph, resolve)) = topSort graph <&> resolve & foldrM evalMatch' Map.empty . catMaybes

catMatchResults :: Map Reference (Either Text MatchResult) -> Map Reference ResourceId
catMatchResults = mapMaybe $ \case { Right (OnwardWith x) -> Just x; _ -> Nothing }

evalMatch' :: AWS m => (Infra, Reference, [Reference]) -> Map Reference (Either Text MatchResult) -> m (Map Reference (Either Text MatchResult))
evalMatch' (n, l, _) ledger = flip (insert l) ledger <$> case reify (catMatchResults ledger) n of
  Left (Missing r) -> return . Left $ "Couldn't lookup without " <> pack (show r)
  Right infra -> match infra <&> \case
    Left NotFound -> Left "Couldn't find any matching infra"
    Left (Ambiguous xs) -> Left $ "Ambiguous result: " <> pack (show xs)
    Right mr -> Right mr

eval :: InfraContext -> (Graph Reference Infra -> AWST' Context (ResourceT (StateT State IO)) a) -> IO a
eval InfraContext{..} task = do
  let Infras{..} = inc_infras
      expression = pack . snd $ splitFileName inc_expressionFile
      tags = [("created-using", "upcast"), ("realm", realmName), ("expression", expression)]
      region = validateRegion regions
  environment <- newEnv region Discover >>= if not inc_verbose then return else \e -> do
    logger <- newLogger Trace stderr
    e & envLogger .~ logger & return
  fmap fst . flip runStateT mempty . runResourceT . runAWST (Context environment tags) $ task resources

-- XXX: I don't think these belong here:

instance ToJSON k => ToJSON (Map Reference k) where
  toJSON mr = object . elems
            . mapWithKey (\c m -> c .= object (map (uncurry (.=)) m))
            . flip (flip foldrWithKey Map.empty) mr $ \(Reference c n) k tmp ->
                insert c ((n, k) : findWithDefault [] c tmp) tmp

instance ToJSON MatchResult where
  toJSON (OnwardWith  id) = object ["OnwardWith" .= id]
  toJSON (NeedsUpdate id) = object ["NeedsUpdate" .= id]
