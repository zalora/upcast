{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Upcast.Infra.Match where

import           Control.Lens

import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.Trans.AWS (MonadAWS, AWSRequest, AWSPager)
import qualified Control.Monad.Trans.AWS as AWS
import           Control.Monad.Trans.Resource

import           Data.Maybe (maybeToList)

import           Data.Conduit (($$), (=$=))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import qualified Network.AWS.EC2 as EC2
import qualified Network.AWS.EC2.Types as EC2
import qualified Network.AWS.ELB as ELB
import qualified Network.AWS.ELB.Types as ELB

import           Upcast.Infra.NixTypes
import           Upcast.Infra.Types hiding (Tags)


data DiscoveryError = NotFound
                    | Ambiguous [Text]
                    deriving Show

type ResourceId = Text
type Tags = [(Text, Text)]


class AWSExtractIds a where
  extractIds :: a -> [ResourceId]

class AWSMatchRequest infra where
  type Rq infra
  matchRequest :: infra -> Tags -> Rq infra


instance AWSMatchRequest Ec2instance where
  type Rq Ec2instance = EC2.DescribeInstances
  matchRequest _ tags = EC2.describeInstances & EC2.di1Filters .~ filters tags

instance AWSExtractIds EC2.DescribeInstancesResponse where
  extractIds resp = do
    rs <- resp ^. EC2.dirReservations
    inst <- rs ^. EC2.rInstances
    return $ inst ^. EC2.i1InstanceId


instance AWSMatchRequest Ec2keypair where
  type Rq Ec2keypair = EC2.DescribeKeyPairs
  matchRequest _ tags = EC2.describeKeyPairs & EC2.dkp1Filters .~ filters tags

instance AWSExtractIds EC2.DescribeKeyPairsResponse where
  extractIds resp = do
    kpi <- resp ^. EC2.dkprKeyPairs
    maybeToList $ kpi ^. EC2.kpiKeyName


instance AWSMatchRequest Ebs where
  type Rq Ebs = EC2.DescribeVolumes
  matchRequest _ tags = EC2.describeVolumes & EC2.dv2Filters .~ filters tags

instance AWSExtractIds EC2.DescribeVolumesResponse where
  extractIds resp = do
    vol <- resp ^. EC2.dvrVolumes
    return $ vol ^. EC2.vVolumeId


instance AWSMatchRequest Ec2sg where
  type Rq Ec2sg = EC2.DescribeSecurityGroups
  matchRequest _ tags = EC2.describeSecurityGroups & EC2.dsg1Filters .~ filters tags

instance AWSExtractIds EC2.DescribeSecurityGroupsResponse where
  extractIds resp = do
    sg <- resp ^. EC2.dsgrSecurityGroups
    return $ sg ^. EC2.sgGroupId


instance AWSMatchRequest Ec2subnet where
  type Rq Ec2subnet = EC2.DescribeSubnets
  matchRequest _ tags = EC2.describeSubnets & EC2.dsFilters .~ filters tags

instance AWSExtractIds EC2.DescribeSubnetsResponse where
  extractIds resp = do
    subnet <- resp ^. EC2.dsrSubnets
    return $ subnet ^. EC2.s1SubnetId


instance AWSMatchRequest Ec2vpc where
  type Rq Ec2vpc = EC2.DescribeVpcs
  matchRequest _ tags = EC2.describeVpcs & EC2.dv1Filters .~ filters tags

instance AWSExtractIds EC2.DescribeVpcsResponse where
  extractIds resp = do
    vpc <- resp ^. EC2.dvrVpcs
    return $ vpc ^. EC2.vpcVpcId


instance AWSMatchRequest Elb where
  type Rq Elb = ELB.DescribeLoadBalancers
  matchRequest Elb{..} _ = ELB.describeLoadBalancers & ELB.dlbLoadBalancerNames .~ [elb_name]

instance AWSExtractIds ELB.DescribeLoadBalancersResponse where
  extractIds resp = do
    elb <- resp ^. ELB.dlbrLoadBalancerDescriptions
    maybeToList $ elb ^. ELB.lbdLoadBalancerName


matchTags ::
  forall infra m.
  (AWSMatchRequest infra,
   AWSExtractIds (AWS.Rs (Rq infra)),
   AWSRequest (Rq infra),
   AWSPager (Rq infra),
   MonadAWS m,
   MonadCatch m,
   MonadError AWS.Error m,
   MonadResource m)
  => infra
  -> Tags
  -> m (Either DiscoveryError ResourceId)
matchTags infra tags = toDiscovery <$> C.runConduit conduit
  where
    conduit = AWS.paginate (matchRequest infra tags) =$=
              CL.concatMap extractIds =$=
              CL.consume

    toDiscovery [one] = Right one
    toDiscovery [] = Left NotFound
    toDiscovery many = Left (Ambiguous many)

filters tags = toFilter <$> ("created-using", "upcast"):tags
  where toFilter (k, v) = EC2.filter' (mconcat ["tag:", k]) & EC2.fValues .~ [v]
