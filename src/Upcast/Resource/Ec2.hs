{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , ScopedTypeVariables
           , ExistentialQuantification
           , FlexibleContexts
           , LambdaCase
           #-}

module Upcast.Resource.Ec2 where

import Control.Applicative
import Control.Monad
import Control.Monad.Free

import Data.Monoid
import Data.Maybe (catMaybes)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson.Types as A
import qualified Data.Vector as V

import Aws.Query (QueryAPIConfiguration(..), castValue)
import qualified Aws.Ec2 as EC2

import Upcast.Resource.Types
import Upcast.Resource.ELB

ec2plan :: (MonadFree ResourceF m, Functor m) => Text -> [EC2.ImportKeyPair] -> Value -> m [(Text, Value)]
ec2plan expressionName keypairs info = do
    mapM_ (\k -> resourceAWSR k "keyFingerprint") keypairs

    vpcA <- fmap mconcat $ forM vpcs $ \vpc -> do
        let (aname, cvpc) = parse vpc $ \(Object obj) -> do
                        aname :: Text <- obj .: "_name"
                        cvpc <- EC2.CreateVpc <$> obj .: "cidrBlock"
                                              <*> pure EC2.Default
                        return (aname, cvpc)
        vpcId <- resourceAWSR cvpc "vpcId"
        return [(aname, vpcId)]

    (wait . EC2.DescribeVpcs) $ fmap snd vpcA
    resourceAWS (EC2.CreateTags (fmap snd vpcA) defTags)

    -- give the internet to VPCs
    forM (fmap snd vpcA) $ \vpcId -> do
      resourceAWS (EC2.ModifyVpcAttribute vpcId (EC2.EnableDnsSupport True))
      resourceAWS (EC2.ModifyVpcAttribute vpcId (EC2.EnableDnsHostnames True))
      gwId <- resourceAWSR EC2.CreateInternetGateway "internetGatewayId"
      resourceAWS (EC2.AttachInternetGateway gwId vpcId)

      rtbId <- resourceAWSR (EC2.DescribeRouteTables vpcId) "routeTableId"
      resourceAWS (EC2.CreateRoute rtbId "0.0.0.0/0" (EC2.GatewayId gwId))

    subnetA <- fmap mconcat $ forM subnets $ \subnet -> do
        let (aname, csubnet) = parse subnet $ \(Object obj) -> do
                        aname :: Text <- obj .: "_name"
                        cidr <- obj .: "cidrBlock"
                        vpc <- obj .: "vpc"
                        zone <- obj .: "zone"
                        let Just vpcId = lookup vpc vpcA
                        return (aname, EC2.CreateSubnet vpcId cidr $ Just zone)
        subnetId <- resourceAWSR csubnet "subnetId"

        return [(aname, subnetId)]

    (wait . EC2.DescribeSubnets) $ fmap snd subnetA
    resourceAWS (EC2.CreateTags (fmap snd subnetA) defTags)

    sgA <- fmap mconcat $ forM secGroups $ \sg -> do
        -- AWS needs more than one transaction for each security group (Create + add rules)
        let (aname, g) = parse sg $ \(Object obj) -> do
                  name :: Text <- obj .: "name"
                  aname :: Text <- obj .: "_name"
                  desc :: Text <- obj .: "description"
                  vpc <- obj .: "vpc"
                  let Just vpcId = lookup vpc vpcA
                  return (aname, EC2.CreateSecurityGroup name desc $ Just vpcId)
        secGroupId <- resourceAWSR g "groupId"

        let r = parse sg $ \(Object obj) -> do
                  rules <- obj .: "rules" :: A.Parser [EC2.IpPermission]
                  return $ EC2.AuthorizeSecurityGroupIngress secGroupId rules
        resourceAWS r

        return [(aname, secGroupId)]

    (wait . flip EC2.DescribeSecurityGroups []) $ fmap snd sgA
    resourceAWS (EC2.CreateTags (fmap snd sgA) defTags)

    volumeA <- fmap mconcat $ forM volumes $ \vol -> do
        let (assocName, name, v) = parse vol $ \(Object obj) -> do
              name :: Text <- obj .: "name"
              assocName :: Text <- obj .: "_name"

              cvol_AvailabilityZone <- obj .: "zone"
              ebd_snapshotId <- fmap (\case "" -> Nothing; x -> Just x) (obj .: "snapshot")
              let ebd_deleteOnTermination = False
              let ebd_volumeType = EC2.Standard
              ebd_volumeSize <- obj .: "size"
              let ebd_encrypted = False

              let cvol_ebs = EC2.EbsBlockDevice{..}

              return $ (assocName, name, EC2.CreateVolume{..})

        volumeId <- resourceAWSR v "volumeId"

        (wait . EC2.DescribeVolumes) [volumeId]
        resourceAWS (EC2.CreateTags [volumeId] (("Name", name):defTags))

        return [(assocName, volumeId)]

    (instanceA :: InstanceA) <- fmap mconcat $ forM instances $ \inst -> do
        let (name, blockDevs, cinst) = parse inst $ \(Object obj) -> do
              String name <- obj .: "targetHost"
              Object ec2 <- obj .: "ec2" :: A.Parser Value
              securityGroupNames <- ec2 .: "securityGroups"
              subnet <- ec2 .: "subnet"
              let Just subnetId = lookup subnet subnetA
              let blockDevs = scast "blockDeviceMapping" (Object ec2) :: Maybe (Map Text Value)

              run_imageId <- ec2 .: "ami"
              let run_count = (1, 1)
              run_instanceType <- ec2 .: "instanceType"
              let run_securityGroupIds = catMaybes $ fmap (flip lookup sgA) securityGroupNames
              -- run_blockDeviceMappings :: [BlockDeviceMapping]
              run_blockDeviceMappings <- return [] -- TODO
              let Just run_subnetId = lookup subnet subnetA
              let run_monitoringEnabled = True
              let run_disableApiTermination = False
              let run_instanceInitiatedShutdownBehavior = EC2.Stop
              run_ebsOptimized <- ec2 .: "ebsOptimized"
              run_keyName <- ec2 .:? "keyPair"
              let run_userData = Just name -- at least need a unique string to prevent substituting one call for many
              let run_kernelId = Nothing
              let run_ramdiskId = Nothing
              let run_clientToken = Nothing
              run_availabilityZone <- ec2 .:? "zone"
             
              return (name, maybe [] Map.toList blockDevs, EC2.RunInstances{..})
        instanceId <- resourceAWSR cinst "instancesSet.instanceId"
        resourceAWS (EC2.CreateTags [instanceId] (("Name", name):defTags))
        return [(name, (instanceId, blockDevs))]

    let instanceIds = fmap (fst . snd) instanceA

    (wait . EC2.DescribeInstanceStatus) instanceIds

    forM (fmap snd instanceA) $ \(avol_instanceId, blockDevs) -> do
      let blockA = (\(mapping, v) -> (mapping, let t = acast "disk" v :: Text
                                                   td = T.drop 4 t
                                                   in case "res-" `T.isPrefixOf` t of
                                                        True -> case lookup td volumeA of
                                                                  Nothing -> error $ mconcat [show td, " not found in volumeA = ", show volumeA]
                                                                  Just x -> x
                                                        False -> error $ mconcat ["can't handle disk: ", T.unpack t])) <$> blockDevs
      forM_ blockA $ \(avol_device, avol_volumeId) -> do
        resourceAWS EC2.AttachVolume{..}

    elbPlan instanceA sgA subnetA elbs

    Array instanceInfos <- aws (EC2.DescribeInstances instanceIds)
    return $ zip (fmap fst instanceA) (V.toList instanceInfos)
  where
    cast :: FromJSON a => Text -> [a]
    cast = (`mcast` info)

    defTags = [("created-using", "upcast"), ("expression", expressionName)]

    vpcs = cast "resources.vpc" :: [Value]
    subnets = cast "resources.subnets" :: [Value]
    secGroups = cast "resources.ec2SecurityGroups" :: [Value]
    volumes = cast "resources.ebsVolumes" :: [Value]
    instances = cast "machines" :: [Value]
    elbs = cast "resources.elbs" :: [Value]


