{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , ScopedTypeVariables
           , ExistentialQuantification
           , FlexibleContexts
           , LambdaCase
           , TupleSections
           , QuasiQuotes
           #-}

module Upcast.Infra.Ec2 where

import Control.Applicative
import Control.Monad
import Control.Monad.Free

import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid
import Data.Traversable (traverse)
import Data.Maybe (catMaybes, maybeToList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Base64 as Base64
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson.Types as A
import qualified Data.Vector as V

import Aws.Query (QueryAPIConfiguration(..), castValue)
import qualified Aws.Ec2 as EC2

import Upcast.IO (expectRight)
import Upcast.Command (fgconsume, Command(..), Local(..))
import Upcast.Interpolate (n)
import Upcast.Infra.Types
import Upcast.Infra.ELB
import Upcast.Infra.NixTypes

-- | read files mentioned in userData for each instance
preReadUserData :: Attrs Ec2instance -> IO UserDataA
preReadUserData instances =
  forAttrs instances $ \_ Ec2instance{..} ->
                        traverse (T.readFile . T.unpack) ec2instance_userData

-- | pre-calculate EC2.ImportKeyPair values while we can do IO
prepareKeyPairs :: Attrs Ec2keypair -> IO (Attrs (Ec2keypair, EC2.ImportKeyPair))
prepareKeyPairs =
  Map.traverseWithKey $ \_ kp@Ec2keypair{..} -> do
    pubkey <- expectRight $ fgconsume $ Cmd Local (mconcat ["ssh-keygen -f ", T.unpack ec2keypair_privateKeyFile, " -y"]) "ssh-keygen"
    return (kp, EC2.ImportKeyPair ec2keypair_name $ T.decodeUtf8 $ Base64.encode pubkey)

createVPC :: (MonadFree InfraF m, Applicative m) => Attrs Ec2vpc -> Tags -> m IDAlist
createVPC vpcs defTags =
  forAttrs vpcs $ \aname Ec2vpc{..} -> do
    vpcId <- aws1 (EC2.CreateVpc ec2vpc_cidrBlock EC2.Default) "vpcId"
    wait $ EC2.DescribeVpcs [vpcId]
    aws_ $ EC2.CreateTags [vpcId] (("Name", aname):defTags)
    return vpcId

internetAccess :: MonadFree InfraF m => IDAlist -> Tags -> m [()]
internetAccess vpcA defTags =
  forM (fmap snd vpcA) $ \vpcId -> do
    -- give the internet to VPC
    aws_ (EC2.ModifyVpcAttribute vpcId (EC2.EnableDnsSupport True))
    aws_ (EC2.ModifyVpcAttribute vpcId (EC2.EnableDnsHostnames True))
    gwId <- aws1 EC2.CreateInternetGateway "internetGatewayId"
    aws_ (EC2.AttachInternetGateway gwId vpcId)
    aws_ (EC2.CreateTags [gwId] defTags)

    rtbId <- aws1 (EC2.DescribeRouteTables vpcId) "routeTableId"
    aws_ (EC2.CreateRoute rtbId "0.0.0.0/0" (EC2.GatewayId gwId))
    aws_ (EC2.CreateTags [rtbId] defTags)

--createSubnets :: (MonadFree InfraF m, Applicative
createSubnets subnets vpcA defTags = do
  subnetA <- forAttrs subnets $ \_ Ec2subnet{..} ->
    let
      Just vpcId = lookupOrId vpcA ec2subnet_vpc
      csubnet = EC2.CreateSubnet vpcId ec2subnet_cidrBlock (Just ec2subnet_region)
    in aws1 csubnet "subnetId"

  unless (null subnetA) $ (wait . EC2.DescribeSubnets) $ fmap snd subnetA
  unless (null subnetA) $ aws_ (EC2.CreateTags (fmap snd subnetA) defTags)

  return subnetA

fromRule :: Rule -> EC2.IpPermission
fromRule Rule{..} = EC2.IpPermission proto (fromIntegral <$> rule_fromPort) (fromIntegral <$> rule_toPort) (maybeToList rule_sourceIp)
  where
    proto = case rule_protocol of
      "tcp" -> EC2.TCP
      "udp" -> EC2.UDP
      "icmp" -> EC2.ICMP
      _ -> EC2.All

createSecurityGroups secGroups vpcA defTags = do
  sgA <- forAttrs secGroups $ \aname Ec2sg{..} -> do
    let vpcId = ec2sg_vpc >>= lookupOrId vpcA
    let g = EC2.CreateSecurityGroup ec2sg_name ec2sg_description vpcId
    secGroupId <- aws1 g "groupId"

    aws_ (EC2.AuthorizeSecurityGroupIngress secGroupId (map fromRule ec2sg_rules))
    return secGroupId

  unless (null sgA) $ (wait . flip EC2.DescribeSecurityGroups []) $ fmap snd sgA
  unless (null sgA) $ aws_ (EC2.CreateTags (fmap snd sgA) defTags)

  return sgA

createEBS volumes defTags =
  forAttrs volumes $ \_ Ebs{..} -> do
    let ebd_snapshotId = ebs_snapshot
    let ebd_deleteOnTermination = True
    let ebd_volumeType = toVolumeType ebs_volumeType
    let ebd_volumeSize = fromIntegral ebs_size
    let ebd_encrypted = False

    let cvol_ebs = EC2.EbsBlockDevice{..}
    let cvol_AvailabilityZone = ebs_zone

    volumeId <- aws1 EC2.CreateVolume{..} "volumeId"

    (wait . EC2.DescribeVolumes) [volumeId]
    aws_ (EC2.CreateTags [volumeId] (("Name", ebs_name):defTags))

    return volumeId
  where
    toVolumeType Gp2 = EC2.GP2SSD
    toVolumeType Standard = EC2.Standard
    toVolumeType (Iop iops) = EC2.IOPSSD (fromIntegral iops)

createInstances instances subnetA sgA defTags userDataA keypairA =
  forAttrs instances $ \name Ec2instance{..} -> do
    let bds = Map.toList (xformMapping <$> ec2instance_blockDeviceMapping)

    let run_blockDeviceMappings = catMaybes (snd <$> bds)
    let run_subnetId = ec2instance_subnet >>= lookupOrId subnetA
    let run_securityGroupIds = catMaybes $ lookupOrId sgA <$> ec2instance_securityGroups
    let run_imageId = ec2instance_ami
    let run_count = (1, 1)
    let run_instanceType = ec2instance_instanceType
    let run_ebsOptimized = ec2instance_ebsOptimized
    let run_keyName = lookupOrId keypairA ec2instance_keyPair
    let run_availabilityZone = Just ec2instance_zone
    let run_iamInstanceProfileARN = ec2instance_instanceProfileARN

    let run_userData = Just (userData name)

    let run_monitoringEnabled = True
    let run_disableApiTermination = False
    let run_instanceInitiatedShutdownBehavior = Nothing
    let run_kernelId = Nothing
    let run_ramdiskId = Nothing
    let run_clientToken = Nothing
    let run_associatePublicIpAddress = True

    awslog $ mconcat [ "instance: ", name
                     , " (", run_instanceType, ", ", run_imageId
                     , ", "
                     , T.pack $ show run_blockDeviceMappings
                     , ")"
                     ]

    instanceId <- aws1 EC2.RunInstances{..} "instancesSet.instanceId"
    aws_ (EC2.CreateTags [instanceId] (("Name", name):defTags))
    return (instanceId, Map.toList ec2instance_blockDeviceMapping)
  where
    -- we can only handle emphemeral volumes at creation time
    xformMapping :: BlockDeviceMapping -> Maybe EC2.BlockDeviceMapping
    xformMapping BlockDeviceMapping{..} =
      case blockDeviceMapping_disk of
       RefLocal _ -> fail "ignore (handled later)"
       RefRemote x | "ephemeral" `T.isPrefixOf` x -> do
                     let bdm_deviceName = blockDeviceMapping_blockDeviceMappingName
                     let bdm_device = EC2.Ephemeral x
                     return EC2.BlockDeviceMapping{..}
                   | otherwise ->
                     fail "ignore (not implemented)"

    textObject = T.decodeUtf8 . toStrict . A.encode . object

    userData name = textObject $ mconcat [ [ "hostname" .= name ]
                                         , maybe [] (Map.toList . fmap String) $ lookup name userDataA
                                         ]

attachEBS :: (MonadFree InfraF m) => InstanceA -> IDAlist -> m [()]
attachEBS instanceA volumeA =
  forM (fmap snd instanceA) $ \(avol_instanceId, blockDevs) -> do
    let blockA = catMaybes $ (\(mapping, v) -> (mapping, ) <$> toVolumeId v) <$> blockDevs
    forM_ blockA $ \(avol_device, avol_volumeId) ->
      aws_ EC2.AttachVolume{..}
  where
    toVolumeId BlockDeviceMapping{..} =
      case blockDeviceMapping_disk of
       RefLocal ref ->
         case lookup ref volumeA of
           Nothing -> error $ mconcat [show ref, " not found in volumeA = ", show volumeA]
           Just ebs -> return ebs
       RefRemote x | "ephemeral" `T.isPrefixOf` x ->
                     fail "ignore (handled already)"
                   | "vol-" `T.isPrefixOf` x ->
                     return x
                   | otherwise ->
                     error $ mconcat ["can't handle disk: ", T.unpack x]

createKeypairs :: (MonadFree InfraF m, Applicative m)
                  => Attrs (Ec2keypair, EC2.ImportKeyPair)
                  -> m IDAlist
createKeypairs cmds =
  forAttrs cmds $ \keypair (Ec2keypair{..}, cmd) -> do
    _ <- aws1 cmd "keyFingerprint"
    return ec2keypair_privateKeyFile


ec2plan :: (MonadFree InfraF m, Functor m, Applicative m)
           => Text
           -> Attrs (Ec2keypair, EC2.ImportKeyPair)
           -> UserDataA
           -> Infras
           -> m (IDAlist, [(Text, Value, Ec2instance)])
ec2plan expressionName keypairs userDataA Infras{..} = do
  keypairA <- createKeypairs keypairs
  vpcA <- createVPC infraEc2vpc defTags
  internetAccess vpcA defTags
  subnetA <- createSubnets infraEc2subnet vpcA defTags
  sgA <- createSecurityGroups infraEc2sg vpcA defTags

  volumeA <- createEBS infraEbs defTags
  instanceA <- createInstances infraEc2instance subnetA sgA defTags userDataA keypairA

  let instanceIds = fmap (fst . snd) instanceA
  when (null instanceIds) $ fail "no instances, plan complete."
  (wait . EC2.DescribeInstanceStatus) instanceIds

  attachEBS instanceA volumeA

  elbPlan ((\(name, (id, _)) -> (name, id)) <$> instanceA) sgA subnetA infraElb

  Array reportedInfos <- aws (EC2.DescribeInstances instanceIds)

  let orderedInstanceNames = fmap fst $ sortBy (compare `on` (fst . snd)) instanceA
      err = error "could not sort instanceIds after DescribeInstances"
      tcast = castValue :: Value -> Maybe Text
      orderedReportedInfos = sortBy (compare `on` (maybe err id . fmap tcast . alookupS "instancesSet.instanceId")) $ V.toList reportedInfos
      orderedInstanceInfos = fmap snd $ sortBy (compare `on` fst) (Map.toList infraEc2instance)
      in return (keypairA, zip3 orderedInstanceNames orderedReportedInfos orderedInstanceInfos)
  where
    defTags = [ ("created-using", "upcast")
              , ("realm", infraRealmName)
              , ("expression", expressionName)
              ]
