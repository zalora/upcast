{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Infracast.Resources.Keypair where

import           Control.Applicative
import           Control.Lens hiding (Context)
import           Control.Lens.Action
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.State (modify)
import           Control.Monad.Trans (liftIO)
import           Control.Monad.Trans.AWS (send)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T (null, isPrefixOf, isSuffixOf, intercalate, take)
import qualified Network.AWS.EC2 as EC2 -- (*)
import           Upcast.IO (expectRight)
import           Infracast.Amazonka
import           Infracast.Types
import           Infracast.NixTypes
import           Upcast.Shell (exec, fgconsume)


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
