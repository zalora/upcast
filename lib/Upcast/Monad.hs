module Upcast.Monad (
-- * re-exports
  module Control.Applicative

, (<=<)
, (>=>)
, ap
, join
, when
, unless
, foldM
, mapM
, mapM_
, mzero

, (<<<)

, liftM
, liftIO

, runResourceT
, MonadResource(..) -- includes liftResourceT

-- *
, mapMBoth
, mapMBoth_
, whenJustM
, sequenceMaybe
) where

import Control.Applicative
import Control.Arrow ((<<<))
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Resource (runResourceT, liftResourceT, MonadResource(..))

mapMBoth :: Monad m => (t -> m a) -> (t, t) -> m (a, a)
mapMBoth f (a, b) = return (,) `ap` f a `ap` f b

mapMBoth_ :: Monad m => (t -> m a) -> (t, t) -> m ()
mapMBoth_ f (a, b) = f a >> f b  >> return ()

whenJustM :: Monad m => m (Maybe a) -> (a -> m b) -> m ()
whenJustM action f = do
  result <- action
  case result of
   Just x -> f x >> return ()
   Nothing -> return ()

sequenceMaybe :: Monad m => [m (Maybe a)] -> m (Maybe a)
sequenceMaybe [] = return Nothing
sequenceMaybe (act:actions) = act >>= maybe (sequenceMaybe actions) (return . Just)
