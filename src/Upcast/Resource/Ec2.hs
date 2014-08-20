{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , ScopedTypeVariables
           , ExistentialQuantification
           , FlexibleContexts
           , LambdaCase
           , TupleSections
           , QuasiQuotes
           #-}

module Upcast.Resource.Ec2 where

import Control.Applicative
import Control.Monad
import Control.Monad.Free

import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid
import Data.Maybe (catMaybes)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString.Lazy (toStrict)
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson.Types as A
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)

import Aws.Query (QueryAPIConfiguration(..), castValue)
import qualified Aws.Ec2 as EC2

import Upcast.Interpolate (n)
import Upcast.ATerm (alookupS)
import Upcast.Resource.Types
import Upcast.Resource.ELB


createVPC vpcs defTags = do
    vpcA <- fmap mconcat $ forM vpcs $ \vpc -> do
        let (aname, cvpc) = parse vpc $ \(Object obj) -> do
                        aname :: Text <- obj .: "_name"
                        cvpc <- EC2.CreateVpc <$> obj .: "cidrBlock"
                                              <*> pure EC2.Default
                        return (aname, cvpc)
        vpcId <- aws1 cvpc "vpcId"
        wait $ EC2.DescribeVpcs [vpcId]
        aws_ $ EC2.CreateTags [vpcId] (("Name", aname):defTags)
        return [(aname, vpcId)]

    return vpcA

internetAccess vpcA defTags = do
    -- give the internet to VPCs
    forM (fmap snd vpcA) $ \vpcId -> do
      aws_ (EC2.ModifyVpcAttribute vpcId (EC2.EnableDnsSupport True))
      aws_ (EC2.ModifyVpcAttribute vpcId (EC2.EnableDnsHostnames True))
      gwId <- aws1 EC2.CreateInternetGateway "internetGatewayId"
      aws_ (EC2.AttachInternetGateway gwId vpcId)
      aws_ (EC2.CreateTags [gwId] defTags)

      rtbId <- aws1 (EC2.DescribeRouteTables vpcId) "routeTableId"
      aws_ (EC2.CreateRoute rtbId "0.0.0.0/0" (EC2.GatewayId gwId))
      aws_ (EC2.CreateTags [rtbId] defTags)

createSubnets subnets vpcA defTags = do
    subnetA <- fmap mconcat $ forM subnets $ \subnet -> do
        let (aname, csubnet) = parse subnet $ \(Object obj) -> do
                        aname :: Text <- obj .: "_name"
                        cidr <- obj .: "cidrBlock"
                        vpc <- obj .: "vpc"
                        zone <- obj .: "zone"
                        let Just vpcId = lookupOrId "vpc-" vpcA vpc
                        return (aname, EC2.CreateSubnet vpcId cidr $ Just zone)
        subnetId <- aws1 csubnet "subnetId"

        return [(aname, subnetId)]

    unless (null subnetA) $ (wait . EC2.DescribeSubnets) $ fmap snd subnetA
    unless (null subnetA) $ aws_ (EC2.CreateTags (fmap snd subnetA) defTags)

    return subnetA

createSecurityGroups secGroups vpcA defTags = do
    sgA <- fmap mconcat $ forM secGroups $ \sg -> do
        -- AWS needs more than one transaction for each security group (Create + add rules)
        let (aname, g) = parse sg $ \(Object obj) -> do
                  name :: Text <- obj .: "name"
                  aname :: Text <- obj .: "_name"
                  desc :: Text <- obj .: "description"
                  vpc <- obj .: "vpc"
                  let vpcId = castText vpc >>= lookupOrId "vpc-" vpcA
                  return (aname, EC2.CreateSecurityGroup name desc vpcId)
        secGroupId <- aws1 g "groupId"

        let r = parse sg $ \(Object obj) -> do
                  rules <- obj .: "rules" :: A.Parser [EC2.IpPermission]
                  return $ EC2.AuthorizeSecurityGroupIngress secGroupId rules
        aws_ r

        return [(aname, secGroupId)]

    unless (null sgA) $ (wait . flip EC2.DescribeSecurityGroups []) $ fmap snd sgA
    unless (null sgA) $ aws_ (EC2.CreateTags (fmap snd sgA) defTags)

    return sgA

createEBS volumes defTags = do
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

        volumeId <- aws1 v "volumeId"

        (wait . EC2.DescribeVolumes) [volumeId]
        aws_ (EC2.CreateTags [volumeId] (("Name", name):defTags))

        return [(assocName, volumeId)]
    return volumeA

createInstances instances subnetA sgA defTags userDataA = do
    (instanceA :: InstanceA) <- fmap mconcat $ forM instances $ \inst -> do
        let (name, blockDevs, cinst) = parse inst $ \(Object obj) -> do
              String name <- obj .: "targetHost"
              Object ec2 <- obj .: "ec2" :: A.Parser Value
              securityGroupNames <- ec2 .: "securityGroups"
              subnet <- ec2 .: "subnet"
              let blockDevs = scast "blockDeviceMapping" (Object ec2) :: Maybe (Map Text Value)

              run_imageId <- ec2 .: "ami"
              let run_count = (1, 1)
              run_instanceType <- ec2 .: "instanceType"
              let run_securityGroupIds = catMaybes $ lookupOrId "sg-" sgA <$> securityGroupNames

              let run_subnetId = castText subnet >>= lookupOrId "subnet-" subnetA
              let run_monitoringEnabled = True
              let run_disableApiTermination = False
              let run_instanceInitiatedShutdownBehavior = EC2.Stop
              run_ebsOptimized <- ec2 .: "ebsOptimized"
              run_keyName <- ec2 .:? "keyPair"
              let run_userData = Just $ userData name
              let run_kernelId = Nothing
              let run_ramdiskId = Nothing
              let run_clientToken = Nothing
              run_availabilityZone <- ec2 .:? "zone"
              run_iamInstanceProfileARN <- castText <$> ec2 .: "instanceProfileARN"

              let run_blockDeviceMappings = catMaybes $ fmap parseBlockDevice (Map.toList $ (\case Just bd -> bd) blockDevs)

              return (name, maybe [] (Map.toList) blockDevs, EC2.RunInstances{..})
        instanceId <- aws1 cinst "instancesSet.instanceId"
        aws_ (EC2.CreateTags [instanceId] (("Name", name):defTags))
        return [(name, (instanceId, blockDevs))]
    return instanceA
  where
    parseBlockDevice :: (Text, Value) -> Maybe EC2.BlockDeviceMapping
    parseBlockDevice (bdm_deviceName, v) = flip A.parseMaybe v $ \(Object obj) -> do
      diskName :: Text <- obj .: "disk"
      when ("res-" `T.isPrefixOf` diskName) $ fail "ignore (handled later)"
      unless ("ephemeral" `T.isPrefixOf` diskName) $ fail "ignore (not implemented)"
      let bdm_device = EC2.Ephemeral diskName

      return EC2.BlockDeviceMapping{..}

    userData hostname = T.decodeUtf8 $ toStrict $ A.encode $ object $ mconcat [ [ "hostname" .= hostname ]
                                                                              , maybe [] (H.toList . fmap String) $ lookup hostname userDataA
                                                                              ]

attachEBS instanceA volumeA = do
    forM (fmap snd instanceA) $ \(avol_instanceId, blockDevs) -> do
      let blockA = catMaybes $ (\(mapping, v) -> (mapping, ) <$> testVol v) <$> blockDevs
      forM_ blockA $ \(avol_device, avol_volumeId) -> do
        aws_ EC2.AttachVolume{..}
  where
    testVol v = let t = acast "disk" v :: Text
                    in case "vol-" `T.isPrefixOf` (acast "disk" v) of
                         True -> return t
                         False -> test t
    test t = let td = T.drop 4 t
                 in do
                    when ("ephemeral" `T.isPrefixOf` t) $ fail "ignore (handled)"
                    case "res-" `T.isPrefixOf` t of
                      True -> case lookup td volumeA of
                                Nothing -> error $ mconcat [show td, " not found in volumeA = ", show volumeA]
                                Just x -> return x
                      False -> error $ mconcat ["can't handle disk: ", T.unpack t]


ec2plan :: (MonadFree ResourceF m, Functor m) => Text -> [EC2.ImportKeyPair] -> Value -> [(Text, HashMap Text Text)] -> m [(Text, Value)]
ec2plan expressionName keypairs info userDataA = do
    mapM_ (\k -> aws1 k "keyFingerprint") keypairs

    vpcA <- createVPC vpcs defTags
    internetAccess vpcA defTags
    subnetA <- createSubnets subnets vpcA defTags
    sgA <- createSecurityGroups secGroups vpcA defTags

    volumeA <- createEBS volumes defTags
    instanceA <- createInstances instances subnetA sgA defTags userDataA

    let instanceIds = fmap (fst . snd) instanceA
    (wait . EC2.DescribeInstanceStatus) instanceIds

    attachEBS instanceA volumeA

    elbPlan instanceA sgA subnetA elbs

    Array instanceInfos <- aws (EC2.DescribeInstances instanceIds)
   
    let orderedInstanceNames = fmap fst $ sortBy (compare `on` (fst . snd)) instanceA
        err = error "could not sort instanceIds after DescribeInstances"
        tcast = castValue :: Value -> Maybe Text
        orderedInfos = sortBy (compare `on` (maybe err id . fmap tcast . alookupS "instancesSet.instanceId")) $ V.toList instanceInfos
        in return $ zip orderedInstanceNames orderedInfos
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

