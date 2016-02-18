{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Upcast.Infra.Resources.Keypair where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Control.Lens hiding (Context) -- (*)
import Control.Lens.Action
import Control.Monad.Trans.AWS (send)
import Control.Monad.State (modify)
import Control.Monad.Reader (MonadReader, asks)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)

import qualified Data.Text as T (null, isPrefixOf, isSuffixOf, intercalate, take)
import Data.Text (Text, pack, unpack)

import qualified Network.AWS.EC2 as EC2 -- (*)

import Upcast.Infra.Types.Amazonka (ResourceId)
import Upcast.Infra.NixTypes -- (*)
import Upcast.Infra.Types.Common

import Upcast.Shell (exec, fgconsume)
import Upcast.IO (expectRight)


keyPrefix :: Tags -> Ec2keypair -> Text
keyPrefix tags Ec2keypair{..} = T.intercalate "-" $
  [ tags ! "created-using"
  , tags ! "realm"
  , tags ! "expression"
  , ec2keypair_name
  ] where (!) = fmap (fromMaybe "") . flip lookup

-- *

request :: AWS m => Action m Ec2keypair EC2.DescribeKeyPairs
request = \f s -> const s <$> f EC2.describeKeyPairs

candidates Ec2keypair{..} keypairs =  do
  prefix <- flip keyPrefix Ec2keypair{..} <$> asks ctxTags
  keypairs ^.. EC2.dkprsKeyPairs . folded . filtered (T.isPrefixOf prefix . view extractId) & return

hashesTo keypair = T.isSuffixOf (hashOf keypair) . view (EC2.kpiKeyName . _Just)

-- XXX: If 'Nothing', "" by mempty on Traversal, don't know how bad this is.
extractId :: Traversal' EC2.KeyPairInfo ResourceId
extractId = EC2.kpiKeyName . _Just


-- *

create :: AWS m => Ec2keypair -> m ResourceId
create Ec2keypair{..} = do
  name <- (<> "-" <> hashOf Ec2keypair{..}) . flip keyPrefix Ec2keypair{..} <$> asks ctxTags
  -- XXX: This filename interface leads to bad hashing; contents can change without updates registering.
  pubkey <- liftIO . expectRight . fgconsume $ exec "ssh-keygen" ["-f", unpack ec2keypair_privateKeyFile, "-y"]
  EC2.importKeyPair name pubkey & send <&> view (EC2.ikprsKeyName . _Just)

update :: AWS m => ResourceId -> Ec2keypair -> m ResourceId
update current Ec2keypair{..} = do
  keypair <- create Ec2keypair{..}
  EC2.deleteKeyPair current & defer . send
  return keypair
