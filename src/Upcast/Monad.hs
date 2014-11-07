module Upcast.Monad (
-- * re-exports
  module Control.Applicative

, (<=<)
, ap
, join
, when
, unless
, foldM
, mapM
, mapM_

, (<<<)

, liftIO

, runResourceT
, MonadResource(..) -- includes liftResourceT

-- *
, mapMBoth
, mapMBoth_
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

