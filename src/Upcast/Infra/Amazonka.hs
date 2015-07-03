{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Upcast.Infra.Amazonka where

import           Control.Applicative
import           Control.Lens hiding ((.=))
import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.Trans.AWS (MonadAWS, AWSRequest, AWSPager, send)
import qualified Control.Monad.Trans.AWS as AWS
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.ByteString.Lazy (toStrict)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, maybeToList)
import           Data.Monoid
import           Data.Proxy
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Data.Traversable (traverse)
import qualified Network.AWS.EC2 as EC2
import qualified Network.AWS.EC2.Types as EC2
import qualified Network.AWS.ELB as ELB
import qualified Network.AWS.ELB.Types as ELB
import           Upcast.IO (expectRight)
import           Upcast.Infra.ELB
import           Upcast.Infra.Match
import           Upcast.Infra.NixTypes
import           Upcast.Infra.Types hiding (Tags)
import           Upcast.Shell (exec, fgconsume)

-- TODO: support intelligent batching/reordering of operations

true = Just (EC2.attributeBooleanValue & EC2.abvValue .~ Just True)

createTags xs tags =
  void . send $ EC2.createTags & EC2.ct1Resources .~ xs
                               & EC2.ct1Tags .~ map (uncurry EC2.tag) tags


createVPC :: MonadAWS m => Tags -> Text -> Ec2vpc -> m ResourceId
createVPC defTags aname Ec2vpc{..} = do
  Just vpc <- view EC2.cvrVpc <$> send (EC2.createVpc ec2vpc_cidrBlock)
  let vpcId = vpc ^. EC2.vpcVpcId

  void . send $ EC2.describeVpcs & EC2.dv1VpcIds .~ [vpcId]
  createTags [vpcId] (("Name", aname):defTags)

  return vpcId

internetAccess :: MonadAWS m => Tags -> ResourceId -> m ()
internetAccess defTags vpcId = do
  void . send $ EC2.modifyVpcAttribute vpcId
    & EC2.mvaEnableDnsHostnames .~ true
    & EC2.mvaEnableDnsSupport .~ true

  Just igw <- view EC2.cigrInternetGateway <$> send EC2.createInternetGateway
  let gwId = igw ^. EC2.igInternetGatewayId

  void . send $ EC2.attachInternetGateway gwId vpcId

  createTags [gwId] defTags

  rtb:_rtbs <- view EC2.drtrRouteTables <$>
          send (EC2.describeRouteTables & EC2.drt2Filters .~ filterIds "vpc-id" [vpcId])

  let Just rtbId = rtb ^. EC2.rtRouteTableId

  void . send $ EC2.createRoute rtbId "0.0.0.0/0" & EC2.crGatewayId .~ Just gwId

  createTags [rtbId] defTags


createSubnet :: MonadAWS m => Tags -> ResourceId -> Text -> Ec2subnet -> m ResourceId
createSubnet defTags vpcId aname Ec2subnet{..} = do
  Just subnet <- view EC2.csrSubnet <$>
                 send (EC2.createSubnet vpcId ec2subnet_cidrBlock
                       & EC2.cs1AvailabilityZone .~ Just ec2subnet_zone)

  let subnetId = subnet ^. EC2.s1SubnetId
  void . send $ matchIds [subnetId] (Proxy :: Proxy Ec2subnet)
  createTags [subnetId] defTags
  return subnetId

ruleToPermission :: Rule -> EC2.IpPermission
ruleToPermission Rule{..} = EC2.ipPermission rule_protocol
                            & EC2.ipFromPort .~ (fromIntegral <$> rule_fromPort)
                            & EC2.ipToPort .~ (fromIntegral <$> rule_toPort)
                            & EC2.ipIpRanges .~ (EC2.ipRange <$> maybeToList rule_sourceIp)

createSecurityGroup :: MonadAWS m => Tags -> ResourceId -> Text -> Ec2sg -> m ResourceId
createSecurityGroup defTags vpcId aname Ec2sg{..} = do
  groupId <- view EC2.csgrGroupId <$>
             send (EC2.createSecurityGroup ec2sg_name ec2sg_description
                   & EC2.csgVpcId .~ Just vpcId)

  void . send $ matchIds [groupId] (Proxy :: Proxy Ec2sg)

  void . send $ EC2.authorizeSecurityGroupIngress
    & EC2.asgiGroupId .~ Just groupId
    & EC2.asgiIpPermissions .~ map ruleToPermission ec2sg_rules

  createTags [groupId] defTags

  return groupId

toVolumeType Gp2 = EC2.Gp2
toVolumeType (Iop _) = EC2.Io1
toVolumeType Standard = EC2.Standard

createVolume :: MonadAWS m => Tags -> Ebs -> m ResourceId
createVolume defTags Ebs{..} = do
  volumeId <- view EC2.cvrVolumeId <$>
              send (EC2.createVolume ebs_zone
                    & EC2.cv1SnapshotId .~ ebs_snapshot
                    & EC2.cv1VolumeType .~ Just (toVolumeType ebs_volumeType)
                    & EC2.cv1Iops .~ (case ebs_volumeType of
                                       Iop n -> Just (fromIntegral n)
                                       _ -> Nothing)
                    & EC2.cv1Size .~ Just (fromIntegral ebs_size))

  void . send $ matchIds [volumeId] (Proxy :: Proxy Ebs)
  createTags [volumeId] (("Name", ebs_name):defTags)
  return volumeId


textObject = T.decodeUtf8 . toStrict . encode . object

userData _ Nothing = Nothing
userData name udata = Just (textObject (mconcat
                                        [ [ "hostname" .= name ]
                                        , maybe [] (Map.toList . fmap String) udata
                                        ]))

-- we can only handle emphemeral volumes at creation time
xformMapping :: BlockDeviceMapping -> Maybe EC2.BlockDeviceMapping
xformMapping BlockDeviceMapping{..} =
  case blockDeviceMapping_disk of
   RefLocal _ -> fail "ignore (handled later)"
   RefRemote x | "ephemeral" `T.isPrefixOf` x ->
                 return (EC2.blockDeviceMapping blockDeviceMapping_blockDeviceMappingName
                         & EC2.bdmVirtualName .~ Just x)
               | otherwise ->
                 fail "ignore (not implemented)"


createInstance :: MonadAWS m
                  => Tags
                  -> Tagged Ec2subnet (Maybe ResourceId)
                  -> Tagged Ec2sg (Maybe ResourceId)
                  -> Maybe (Attrs Text) -- | UserData
                  -> Tagged Ec2keypair (Maybe ResourceId)
                  -> Text
                  -> Ec2instance
                  -> m ResourceId
createInstance defTags
               (unTagged -> subnetId)
               (unTagged -> securityGroupId)
               udata
               (unTagged -> keyName)
               name
               Ec2instance{..} = do
  let bds = Map.toList (xformMapping <$> ec2instance_blockDeviceMapping)

  inst:_ <- view EC2.rirInstances <$> send (
    EC2.runInstances ec2instance_ami 1 1
    & EC2.riSubnetId .~ subnetId
    & EC2.riSecurityGroupIds .~ maybeToList securityGroupId
    & EC2.riUserData .~ userData name udata
    & EC2.riKeyName .~ keyName
    & EC2.riEbsOptimized .~ Just ec2instance_ebsOptimized
    & EC2.riPlacement .~ Just (EC2.placement
                               & EC2.pAvailabilityZone .~ Just ec2instance_zone)
    & EC2.riIamInstanceProfile .~ Just (EC2.iamInstanceProfileSpecification
                                        & EC2.iipsArn .~ ec2instance_instanceProfileARN)
    & EC2.riNetworkInterfaces .~ [EC2.instanceNetworkInterfaceSpecification
                                  & EC2.inisAssociatePublicIpAddress .~ Just True]
    & EC2.riMonitoring .~ Just (EC2.runInstancesMonitoringEnabled True)
    & EC2.riBlockDeviceMappings .~ catMaybes (snd <$> bds))

  let instanceId = inst ^. EC2.i1InstanceId

  createTags [instanceId] (("Name", name):defTags)

  return instanceId



attachVolume :: MonadAWS m
                => Tagged Ec2instance ResourceId
                -> Tagged Ebs ResourceId
                -> Text
                -> m ()
attachVolume (unTagged -> instanceId) (unTagged -> volumeId) device =
  void . send $ EC2.attachVolume volumeId instanceId device

createELB = undefined

plan expressionName Infras{..} =
  let
    defTags = [ ("created-using", "upcast")
              , ("realm", infraRealmName)
              , ("expression", expressionName)
              ]
  in do
    vpcA <- forAttrs infraEc2vpc (createVPC defTags)
    forM_ vpcA (\(_, vpcId) -> internetAccess defTags vpcId)
    vpcId <- undefined -- magic lookup
    subnetA <- forAttrs infraEc2subnet (createSubnet defTags vpcId)
    sgA <- forAttrs infraEc2sg (createSecurityGroup defTags vpcId)
    volumeA <- forAttrs infraEbs (\_ -> createVolume defTags)
    subnetId <- undefined
    securityGroupId <- undefined
    udata <- undefined
    keyName <- undefined
    instanceA <- forAttrs infraEc2instance (createInstance defTags subnetId securityGroupId udata keyName)

    return ()
