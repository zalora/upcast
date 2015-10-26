{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}

module Upcast.Infra.Types.Common
( AWS
, Reference(..)
, Context(..)
, State(..)
, Tags
) where

import Control.Applicative (Applicative(..))
import Control.Lens ((<&>))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.AWS (Env)
import Control.Monad.Trans.Resource (MonadBaseControl, MonadResource)
import Data.Hashable (Hashable(..))
import Data.Monoid (Monoid(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Network.AWS.EC2 as EC2 (Instance)
import Network.AWS.Env (HasEnv(..))
import Upcast.Infra.NixTypes (Ec2keypair)

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
