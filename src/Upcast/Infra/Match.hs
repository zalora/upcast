{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Upcast.Infra.Match where

import           Prelude hiding (sequence)

import           Control.Applicative
import           Control.Lens hiding ((.=))

import           Control.Monad hiding (sequence)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.Trans.AWS (MonadAWS, AWSRequest, AWSPager)
import qualified Control.Monad.Trans.AWS as AWS
import           Control.Monad.Trans.Resource

import           Data.Aeson (ToJSON, Value, object, (.=))
import           Data.Maybe (maybeToList)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Map as Map
import           Data.Traversable (sequence, traverse)
import           GHC.Generics

import           Data.Conduit (($$), (=$=))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import qualified Network.AWS.EC2 as EC2
import qualified Network.AWS.EC2.Types as EC2
import qualified Network.AWS.ELB as ELB
import qualified Network.AWS.ELB.Types as ELB

import           Upcast.Infra.NixTypes
import           Upcast.Infra.Types
import           Upcast.IO (expectRight)
import           Upcast.Infra.AmazonkaTypes (validateRegion)


data DiscoveryError = NotFound
                    | Ambiguous [Text]
                    deriving (Show, Generic)
instance ToJSON DiscoveryError

type ResourceId = Text

type CanMatch infra =
  ( AWSMatchRequest infra
  , AWSExtractIds (AWS.Rs (Rq infra))
  , AWSRequest (Rq infra)
  , AWSPager (Rq infra)
  )

class AWSExtractIds a where
  extractIds :: a -> [ResourceId]

class AWSMatchRequest infra where
  type Rq infra
  matchRequest :: infra -> Tags -> Rq infra
  matchIds :: [ResourceId] -> proxy infra -> Rq infra

instance AWSMatchRequest Ec2instance where
  type Rq Ec2instance = EC2.DescribeInstances
  matchRequest _ tags = EC2.describeInstances & EC2.di1Filters .~ filters tags
  matchIds ids _ = EC2.describeInstances & EC2.di1InstanceIds .~ ids

instance AWSExtractIds EC2.DescribeInstancesResponse where
  extractIds resp = do
    rs <- resp ^. EC2.dirReservations
    inst <- rs ^. EC2.rInstances
    return $ inst ^. EC2.i1InstanceId


instance AWSMatchRequest Ec2keypair where
  type Rq Ec2keypair = EC2.DescribeKeyPairs
  matchRequest _ tags = EC2.describeKeyPairs & EC2.dkp1Filters .~ filters tags
  matchIds ids _ = EC2.describeKeyPairs & EC2.dkp1Filters .~ filterIds "key-name" ids

instance AWSExtractIds EC2.DescribeKeyPairsResponse where
  extractIds resp = do
    kpi <- resp ^. EC2.dkprKeyPairs
    maybeToList $ kpi ^. EC2.kpiKeyName


instance AWSMatchRequest Ebs where
  type Rq Ebs = EC2.DescribeVolumes
  matchRequest Ebs{..} tags = EC2.describeVolumes &
                              EC2.dv2Filters .~ filters (("Name", ebs_name):tags)
  matchIds ids _ = EC2.describeVolumes & EC2.dv2VolumeIds .~ ids

instance AWSExtractIds EC2.DescribeVolumesResponse where
  extractIds resp = do
    vol <- resp ^. EC2.dvrVolumes
    return $ vol ^. EC2.vVolumeId


instance AWSMatchRequest Ec2sg where
  type Rq Ec2sg = EC2.DescribeSecurityGroups
  matchRequest _ tags = EC2.describeSecurityGroups & EC2.dsg1Filters .~ filters tags
  matchIds ids _ = EC2.describeSecurityGroups & EC2.dsg1GroupIds .~ ids

instance AWSExtractIds EC2.DescribeSecurityGroupsResponse where
  extractIds resp = do
    sg <- resp ^. EC2.dsgrSecurityGroups
    return $ sg ^. EC2.sgGroupId


instance AWSMatchRequest Ec2subnet where
  type Rq Ec2subnet = EC2.DescribeSubnets
  matchRequest _ tags = EC2.describeSubnets & EC2.dsFilters .~ filters tags
  matchIds ids _ = EC2.describeSubnets & EC2.dsSubnetIds .~ ids

instance AWSExtractIds EC2.DescribeSubnetsResponse where
  extractIds resp = do
    subnet <- resp ^. EC2.dsrSubnets
    return $ subnet ^. EC2.s1SubnetId


instance AWSMatchRequest Ec2vpc where
  type Rq Ec2vpc = EC2.DescribeVpcs
  matchRequest _ tags = EC2.describeVpcs & EC2.dv1Filters .~ filters tags
  matchIds ids _ = EC2.describeVpcs & EC2.dv1VpcIds .~ ids

instance AWSExtractIds EC2.DescribeVpcsResponse where
  extractIds resp = do
    vpc <- resp ^. EC2.dvrVpcs
    return $ vpc ^. EC2.vpcVpcId


instance AWSMatchRequest Elb where
  type Rq Elb = ELB.DescribeLoadBalancers
  matchRequest Elb{..} _ = ELB.describeLoadBalancers & ELB.dlbLoadBalancerNames .~ [elb_name]
  matchIds ids _ = ELB.describeLoadBalancers & ELB.dlbLoadBalancerNames .~ ids

instance AWSExtractIds ELB.DescribeLoadBalancersResponse where
  extractIds resp = do
    elb <- resp ^. ELB.dlbrLoadBalancerDescriptions
    maybeToList $ elb ^. ELB.lbdLoadBalancerName


instance AWSPager EC2.DescribeVpcs where page _ _ = Nothing
instance AWSPager EC2.DescribeSubnets where page _ _ = Nothing
instance AWSPager EC2.DescribeSecurityGroups where page _ _ = Nothing
instance AWSPager EC2.DescribeKeyPairs where page _ _ = Nothing
instance AWSPager EC2.DescribeVolumes where page _ _ = Nothing


matchTags ::
  forall infra m.
  (CanMatch infra,
   MonadAWS m,
   MonadCatch m,
   MonadError AWS.Error m,
   MonadResource m)
  => infra
  -> Tags
  -> m [ResourceId]
matchTags infra tags = C.runConduit conduit
  where
    conduit = AWS.paginate (matchRequest infra tags) =$=
              CL.concatMap extractIds =$=
              CL.consume

toDiscovery [one] = Right one
toDiscovery [] = Left NotFound
toDiscovery many = Left (Ambiguous many)

discover i t = toDiscovery <$> matchTags i t

filters tags = toFilter <$> tags --("created-using", "upcast"):tags
  where toFilter (k, v) = EC2.filter' (mconcat ["tag:", k]) & EC2.fValues .~ [v]

filterIds tag ids = [EC2.filter' tag & EC2.fValues .~ ids]

matchInfras :: Infras -> IO Value
matchInfras infras@Infras{..} = object <$> do
  env <- AWS.getEnv region AWS.Discover
  expectRight $ AWS.runAWST env $
    sequence [ ("vpc" .=)      <$> matchWithName infraEc2vpc
             , ("subnet" .=)   <$> match infraEc2subnet
             , ("sg" .=)       <$> match infraEc2sg
             , ("keypair" .=)  <$> match infraEc2keypair
             , ("ebs" .=)      <$> match infraEbs
             , ("instance" .=) <$> matchWithName infraEc2instance
             , ("elb" .=)      <$> match infraElb
             ]
  where
    match ::
      (CanMatch a, Applicative f, MonadCatch f, MonadError AWS.Error f, AWS.MonadAWS f)
      => Attrs a -> f (Attrs (Either DiscoveryError ResourceId))
    match = traverse (`discover` [("realm", infraRealmName)])

    matchWithName ::
      (CanMatch a, Applicative f, MonadCatch f, MonadError AWS.Error f, AWS.MonadAWS f)
      => Attrs a -> f (Attrs (Either DiscoveryError ResourceId))
    matchWithName = Map.traverseWithKey
                   (\k v -> discover v [("realm", infraRealmName), ("Name", k)])

    region = validateRegion infras
