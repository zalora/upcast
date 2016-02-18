{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TupleSections #-}

module Upcast.Infra.Graph
( Graph(..)
, splice
) where

import Prelude hiding (filter)
import Control.Lens (Lens, over, Field1(_1), Field2(_2), Field3(_3))
import Control.Monad (join)
import Data.Array (array, assocs)
import Data.Bifunctor (Bifunctor(..))
import Data.Bifunctor.Flip (Flip(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable (Bitraversable(..))
import Data.Maybe (fromMaybe)
import Data.Graph (Vertex, vertices)
import qualified Data.Graph (Graph)
import Data.Witherable (Witherable(..))

newtype Graph l v = Graph { getGraph :: (Data.Graph.Graph, Vertex -> Maybe (v, l, [l])) }

_graph :: Lens (Graph l0 v0)
               (Graph l1 v1)
               (Data.Graph.Graph, Vertex -> Maybe (v0, l0, [l0]))
               (Data.Graph.Graph, Vertex -> Maybe (v1, l1, [l1]))
_graph = \f g -> Graph <$> f (getGraph g)

instance Bifunctor Graph where
  bimap :: (l0 -> l1) -> (v0 -> v1) -> Graph l0 v0 -> Graph l1 v1
  bimap f g = over (_graph . _2) (fmap bimap' .)
    where bimap' = over _3 (fmap f) . over _2 f . over _1 g

instance Functor (Graph l) where
  fmap :: (v0 -> v1) -> Graph l v0 -> Graph l v1
  fmap = second

instance Bifoldable Graph where
  bifoldMap :: Monoid m => (l -> m) -> (v -> m) -> Graph l v -> m
  bifoldMap fl fv (Graph (g,h)) = foldMap (maybe mempty (\(v,l,_) -> fl l `mappend` fv v) . h) $ vertices g

instance Foldable (Graph l) where
  foldMap :: Monoid m => (v -> m) -> Graph l v -> m
  foldMap = bifoldMap mempty

instance Bitraversable Graph where
  bitraverse :: Applicative f => (l0 -> f l1) -> (v0 -> f v1) -> Graph l0 v0 -> f (Graph l1 v1)
  bitraverse f g (Graph (graph, h)) = Graph . (graph,) <$> bitraverse'
    where bitraverse' = fmap (fmap join . flip lookup)
                      . (traverse . traverse . traverse $ \(v0,l0,l0s) -> (,,)
                                                                      <$> g v0
                                                                      <*> f l0
                                                                      <*> traverse f l0s)
                      $ (,) <*> h <$> vertices graph

instance Traversable (Graph l) where
  traverse :: Applicative f => (v0 -> f v1) -> Graph l v0 -> f (Graph l v1)
  traverse = bitraverse pure

class Bitraversable p => Biwitherable p where
  biwither :: Applicative f => (a -> f (Maybe c)) -> (b -> f (Maybe d)) -> p a b -> f (p c d)

instance Biwitherable Graph where
  biwither :: Applicative f => (l0 -> f (Maybe l1)) -> (v0 -> f (Maybe v1)) -> Graph l0 v0 -> f (Graph l1 v1)
  biwither = fmap (fmap $ fmap snip) . bitraverse
    where snip :: Graph (Maybe l) (Maybe v) -> Graph l v
          snip (Graph (g,h)) = Graph (graph, look) where
            magic (Just _, Just _, _) = True; magic _ = False
            keep = filter (fromMaybe False . fmap magic . h) $ vertices g
            graph = array (minimum keep, maximum keep)
                          [ (i, filter (`elem` keep) j)
                          | (i, j) <- assocs g
                          , i `elem` keep
                          ]
            look vertex = case h vertex of
              Just (Just x, Just y, z) -> Just (x, y, catMaybes z)
              _ -> Nothing

instance Witherable (Graph l) where
  wither :: Applicative f => (v0 -> f (Maybe v1)) -> Graph l v0 -> f (Graph l v1)
  wither = biwither (pure . Just)

instance Biwitherable p => Witherable (Flip p b) where
  wither :: Applicative f => (a0 -> f (Maybe a1)) -> Flip p b a0 -> f (Flip p b a1)
  wither f = fmap Flip . biwither f (pure . Just) . runFlip

splice :: Graph l v -> Graph l (l, v)
splice = over (_graph . _2) . fmap . fmap $ \(x,y,z) -> ((y,x),y,z)
