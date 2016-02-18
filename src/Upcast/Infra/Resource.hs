{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}

module Upcast.Infra.Resource
( Resource(..)
, Infra
) where

import           Control.Applicative
import           Control.Lens hiding (Context)
import           Control.Lens.Action
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.State (modify)
import           Control.Monad.Trans.AWS (Rs, AWSPager(..))
import           Data.Aeson.Types (Value(..), ToJSON(..), FromJSON(..), Parser(..), parseMaybe, (.:), (.=))
import           Data.Graph (vertices, graphFromEdges', topSort)
import           Data.Map (Map)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Data.Traversable (mapM)
import           Data.Witherable (Witherable(..))
import qualified Network.AWS.EC2 as EC2
import qualified Network.AWS.ELB as ELB
import           Prelude hiding (filter, mapM)
import           Upcast.Infra.Amazonka
import           Upcast.Infra.Graph
import           Upcast.Infra.NixTypes
import           Upcast.Infra.Types

import qualified Upcast.Infra.Resources.ELB as ELB
import qualified Upcast.Infra.Resources.Instance as Instance
import qualified Upcast.Infra.Resources.Keypair as Keypair
import qualified Upcast.Infra.Resources.Subnet as Subnet
import qualified Upcast.Infra.Resources.EBS as EBS
import qualified Upcast.Infra.Resources.SecurityGroup as SG
import qualified Upcast.Infra.Resources.VPC as VPC

-- *

class Matcher a b | a -> b, b -> a where
  -- | The API action called to match the infra 'a'. Usually an instance of
  -- Amazonka's 'AWSRequest'; such that 'Rs (Rq a)' is defined.
  type Rq a

  -- | Construct a request for remote resources which may match the local
  -- resource. Note that post-request processing is possible with
  -- 'candidates'.
  request :: AWS m => Action m a (Rq a)

  -- | Any required post-'request' filters, operating on remote resources. It's
  -- best to delegate filters to 'request' when possible, but this is not
  -- always so, e.g. prefix matching.
  --
  -- XXX: It would be more consistent to denote this as an 'Action', e.g.
  -- > AWS m => a -> Action m (Rs (Rq a)) [b]
  -- with the added advantage of terser definitions; '= EC2.dvrsVPCs' etc.
  candidates :: AWS m => a -> Rs (Rq a) -> m [b]

  -- | Validate whether the remote resource precisely corresponds to the local
  -- resource/infra.
  hashesTo :: a -> (b -> Bool)

  -- | Given a remote resource, extract its identifier.
  extractId :: Traversal' b ResourceId

match' :: AWS m => AWSPager (Rq a) => Matcher a b => a -> m (Either DiscoveryError MatchResult)
match' infra = do
  infras <- infra ^! request >>= gimme >>= fmap concat . mapM (candidates infra)
  return $ case filter (hashesTo infra) infras of
    [one] -> Right . OnwardWith $ one ^. extractId
    []    -> fmap NeedsUpdate . toDiscovery $ view extractId <$> infras
    many  -> Left . Ambiguous $ view extractId <$> many

class Resource a where
  -- | Search for an existing copy of the resource. Interpretation:
  --
  --  Right (OnwardWith (id :: ResourceId))
  --    ~ A candidate resource was identified, and does not require 'update'.
  --  Right (NeedsUpdate (id :: ResourceId))
  --    ~ A single candidate resource was identified, but it needs to be
  --      'update'd to match the resource.
  --  Left (Ambiguous (xs :: [ResourceId])
  --    ~ Multiple candidate resources were identified.
  --  Left NotFound
  --    ~ No existing version of the resource was identified.
  --
  -- XXX: It would be more informative to disambiguate 'Ambiguous' results
  -- with and without matching hashes.
  match  :: AWS m => a -> m (Either DiscoveryError MatchResult)
  default match :: (AWS m, AWSPager (Rq a), Matcher a b) => a -> m (Either DiscoveryError MatchResult)
  match = match'

  -- | Create the resource, yield its identifier.
  create :: AWS m => a -> m ResourceId

  -- | Update an identified resource.
  update :: AWS m => ResourceId -> a -> m ResourceId

  -- | Substitute any 'RefLocal' references in 'a' for 'RefRemote', using some
  -- correspondence between namespaced 'RefLocal' and 'ResourceId's. If some
  -- local reference cannot be resolved; Left (Missing ~ Reference).
  reify  :: Map Reference ResourceId -> (a -> Either Missing a)
  reify = const return

instance AWSPager EC2.DescribeVPCs where page _ _ = Nothing

instance Matcher Ec2vpc EC2.VPC where
  type Rq Ec2vpc = EC2.DescribeVPCs
  request = VPC.request
  candidates = VPC.candidates
  hashesTo = VPC.hashesTo
  extractId = VPC.extractId

instance Resource Ec2vpc where
  create = VPC.create
  update = VPC.update

instance AWSPager EC2.DescribeSecurityGroups where page _ _ = Nothing

instance Matcher Ec2sg EC2.SecurityGroup where
  type Rq Ec2sg = EC2.DescribeSecurityGroups
  request = SG.request
  candidates = SG.candidates
  hashesTo = SG.hashesTo
  extractId = SG.extractId

instance Resource Ec2sg where
  create = SG.create
  update = SG.update
  reify = SG.reify

instance Resource Ec2sgruleset where
  match = SG.matchRuleset
  create = SG.createRuleset
  update = SG.updateRuleset
  reify = SG.reifyRuleset

instance Matcher Ebs EC2.Volume where
  type Rq Ebs = EC2.DescribeVolumes
  request = EBS.request
  candidates = EBS.candidates
  hashesTo = EBS.hashesTo
  extractId = EBS.extractId

instance Resource Ebs where
  create = EBS.create
  update = EBS.update

instance AWSPager EC2.DescribeSubnets where page _ _ = Nothing

instance Matcher Ec2subnet EC2.Subnet where
  type Rq Ec2subnet = EC2.DescribeSubnets
  request = Subnet.request
  candidates = Subnet.candidates
  hashesTo = Subnet.hashesTo
  extractId = Subnet.extractId

instance Resource Ec2subnet where
  create = Subnet.create
  update = Subnet.update
  reify = Subnet.reify

instance Matcher Ec2keypair EC2.KeyPairInfo where
  type Rq Ec2keypair = EC2.DescribeKeyPairs
  request = Keypair.request
  candidates = Keypair.candidates
  hashesTo = Keypair.hashesTo
  extractId = Keypair.extractId

instance AWSPager EC2.DescribeKeyPairs where page _ _ = Nothing

instance Resource Ec2keypair where
  create = Keypair.create
  update = Keypair.update

  match Ec2keypair{..} = do
    prefix <- asks ctxTags <&> flip Keypair.keyPrefix Ec2keypair{..}
    addKeypair $ Ec2keypair{..} { ec2keypair_name = prefix <> "-" <> hashOf Ec2keypair{..} }
    match' Ec2keypair{..}
      where
        addKeypair :: AWS m => Ec2keypair -> m ()
        addKeypair key = modify $ \State{..} -> State{..} { stateKeyPairs = key : stateKeyPairs }


instance Matcher Ec2instance EC2.Instance where
  type Rq Ec2instance = EC2.DescribeInstances
  request = Instance.request
  candidates = Instance.candidates
  hashesTo = Instance.hashesTo
  extractId = Instance.extractId

instance Resource Ec2instance where
  create = Instance.create
  update = Instance.update
  reify = Instance.reify
  match = Instance.match

instance Matcher Elb (Tags, ELB.LoadBalancerDescription) where
  type Rq Elb = ELB.DescribeLoadBalancers
  request = ELB.request
  candidates = ELB.candidates
  hashesTo = ELB.hashesTo
  extractId = ELB.extractId

instance Resource Elb where
  create = ELB.create
  update = ELB.update
  reify = ELB.reify

instance Resource Elbinstanceset where
  match = ELB.matchInstanceSet
  create = ELB.createInstanceSet
  update = ELB.updateInstanceSet
  reify = ELB.reifyInstanceSet

-- * One existential to wrap them all

data Infra = forall a. (FromJSON a, Resource a, Show a) => Infra a

instance Resource Infra where
  match (Infra infra) = match infra
  create (Infra infra) = create infra
  update current (Infra infra) = update current infra
  reify ledger (Infra infra) = Infra <$> reify ledger infra

-- XXX: These choices are strictly ordered because some JSON representations
-- are ambiguous with respect to each other absent '_name'. Something like
-- 'o .: _name ~ "ec2-subnet" *> parseJSON value :: ...' might help, but I
-- haven't tried it yet.
instance FromJSON Infra where
  parseJSON value = (Infra <$> (parseJSON value :: Parser Ebs))
                <|> (Infra <$> (parseJSON value :: Parser Ec2instance))
                <|> (Infra <$> (parseJSON value :: Parser Ec2keypair))
                <|> (Infra <$> (parseJSON value :: Parser Ec2subnet))
                <|> (Infra <$> (parseJSON value :: Parser Ec2sg))
                <|> (Infra <$> (parseJSON value :: Parser Ec2sgruleset))
                <|> (Infra <$> (parseJSON value :: Parser Ec2vpc))
                <|> (Infra <$> (parseJSON value :: Parser Elb))
                <|> (Infra <$> (parseJSON value :: Parser Elbinstanceset))

instance Show Infra where
  showsPrec n (Infra a) = showsPrec n a
