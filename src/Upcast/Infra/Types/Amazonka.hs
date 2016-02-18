{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Upcast.Infra.Types.Amazonka
( AWS
, State(..)
, Context(..)
, Tags
, ResourceId
, toEc2Tags
, true
, _filter
, isTag
, hashTagIs
, hashOf
, gimme
, defer
, validateRegion
) where

import           Control.Lens hiding (Context)
import           Control.Monad
import           Control.Monad.Catch (MonadCatch, MonadThrow)
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.State (MonadState)
import           Control.Monad.Trans.AWS (Env, Rs, AWSPager(..), send, await, paginate, AWST, runAWST)
import           Control.Monad.Trans.Resource
import           Data.Conduit (runConduit, tryC, fuse)
import           Data.Conduit.List (consume)
import           Data.Hashable (Hashable(..))
import           Data.Monoid (Monoid(..), (<>))
import           Data.Text (Text, pack, unpack)
import qualified Network.AWS.EC2 as EC2
import           Network.AWS.Env (HasEnv(..))
import           Network.AWS.Types (Error)
import           Network.AWS.Types (Region(..))
import           Upcast.Infra.NixTypes

-- *

type AWS m = ( Functor m
             , Applicative m
             , MonadCatch m
             , MonadThrow m
             , MonadResource m
             , MonadBaseControl IO m
             , MonadReader Context m
             , MonadState State m
             )

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

type ResourceId = Text

type Tags = [(Text, Text)]

toEc2Tags :: AWS m => [ResourceId] -> Tags -> m ()
toEc2Tags xs tags =
  void . send $ EC2.createTags & EC2.cResources .~ xs
                               & EC2.cTags .~ map (uncurry EC2.tag) tags

-- *

-- "interface EC2APIAttributeServiceValuesFactoryFactory" -- @vlad
true :: Maybe EC2.AttributeBooleanValue
true = Just (EC2.attributeBooleanValue & EC2.abvValue ?~ True)

_filter :: Getter (Text, Text) EC2.Filter
_filter = \f t@(k, v) -> const t <$> f (EC2.filter' ("tag:" <> k) & EC2.fValues .~ [v])

isTag :: Text -> Text -> (EC2.Tag -> Bool)
isTag key value tag = (tag ^. EC2.tagKey) == key && (tag ^. EC2.tagValue) == value

hashOf :: Hashable h => h -> Text
hashOf = pack . show . hash

hashTagIs :: Hashable h => h -> (EC2.Tag -> Bool)
hashTagIs = isTag "hash" . hashOf -- # yolo

gimme :: (AWS m, AWSPager rq) => rq -> m [Rs rq]
gimme = either (throwM :: MonadThrow m => Error -> m a) return
    <=< runConduit . tryC . flip fuse consume . paginate

defer :: AWS m => AWST (ResourceT IO) a -> m ReleaseKey
defer act = asks ctxEnv >>= liftResourceT . register . void . runResourceT . flip runAWST act

-- *

readRegion :: Text -> Region
readRegion s = case unpack s of
  "eu-west-1"          -> Ireland
  "eu-central-1"       -> Frankfurt
  "ap-northeast-1"     -> Tokyo
  "ap-southeast-1"     -> Singapore
  "ap-southeast-2"     -> Sydney
  "cn-north-1"         -> Beijing
  "us-east-1"          -> NorthVirginia
  "us-west-2"          -> Oregon
  "us-west-1"          -> NorthCalifornia
  "us-gov-west-1"      -> GovCloud
  "fips-us-gov-west-1" -> GovCloudFIPS
  "sa-east-1"          -> SaoPaulo
  unk                  -> error ("unknown region " ++ unk)

validateRegion :: [Text] -> Region
validateRegion [region] = readRegion region
validateRegion many = error $ mconcat
  [ "can only operate with expressions that "
  , "do not span multiple EC2 regions, given: "
  , show many
  ]
