{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverlappingInstances
           , RecordWildCards
           , LambdaCase
           , RecordWildCards
           , NamedFieldPuns
           , OverloadedStrings
           #-}


module Aws.Ec2.Experimental where

import GHC.Generics
import Data.Text (Text)
import Data.Time.Clock

data Reservation = Reservation
                    { rs_reservationId :: Text
                    , rs_ownerId :: Text
                    , rs_requesterId :: Maybe Text
                    , rs_groups :: [SecurityGroupItem]
                    , rs_instances :: [RunningInstance]
                    } deriving (Show, Eq, Ord)

data SecurityGroupItem = SecurityGroupItem
                   { sg_groupId :: Text
                   , sg_groupName :: Text
                   } deriving (Show, Eq, Ord, Generic)

data InstanceState = InstanceState -- http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-ItemType-InstanceStateType.html
                   { is_code :: Integer
                   , is_name :: Text
                   } deriving (Show, Eq, Ord)

data PlacementResponse = PlacementResponse -- http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-ItemType-PlacementResponseType.html
                       { pr_availabilityZone :: Text
                       , pr_groupName :: Text
                       , pr_tenancy :: Text -- default | dedicated
                       } deriving (Show, Eq, Ord)

data StateReason = StateReason -- http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-ItemType-StateReasonType.html
                 { sr_code :: Text
                 , sr_message :: Text
                 } deriving (Show, Eq, Ord)

data ResourceTag = ResourceTag -- http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-ItemType-ResourceTagSetItemType.html
                 { rt_key :: Text
                 , rt_value :: Text
                 } deriving (Show, Eq, Ord)

data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment -- http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-ItemType-InstanceNetworkInterfaceAttachmentType.html
                                        { niat_attachmentId :: Text
                                        , niat_deviceIndex :: Integer
                                        , niat_status :: Text -- attaching | attached | detaching | detached
                                        , niat_attachTime :: UTCTime
                                        , niat_deleteOnTermination :: Bool
                                        } deriving (Show, Eq, Ord)

data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation -- http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-ItemType-InstanceNetworkInterfaceAssociationType.html
                                         { nias_publicIp :: Text
                                         , nias_publicDnsName :: Text
                                         , nias_ipOwnerId :: Text
                                         } deriving (Show, Eq, Ord)

data InstancePrivateIpAddress = InstancePrivateIpAddress -- http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-ItemType-InstancePrivateIpAddressesSetItemType.html
                              { pi_privateIpAddress :: Text
                              , pi_privateDnsName :: Text
                              , pi_primary :: Bool
                              , pi_association :: InstanceNetworkInterfaceAssociation
                              } deriving (Show, Eq, Ord)

data InstanceNetworkInterface = InstanceNetworkInterface -- http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-ItemType-InstanceNetworkInterfaceSetItemType.html
                              { ni_networkInterfaceId :: Text
                              , ni_subnetId :: Text
                              , ni_vpcId :: Text
                              , ni_description :: Text
                              , ni_ownerId :: Text
                              , ni_status :: Text -- available | attaching | in-use | detaching
                              , ni_macAddress :: Text
                              , ni_privateIpAddress :: Text
                              , ni_privateDnsNmae :: Text
                              , ni_sourceDestCheck :: Bool
                              , ni_groupSet_item :: [SecurityGroupItem]
                              , ni_attachment :: InstanceNetworkInterfaceAttachment
                              , ni_association :: InstanceNetworkInterfaceAssociation
                              , ni_privateIpAddressesSet :: [InstancePrivateIpAddress]
                              } deriving (Show, Eq, Ord)

data IamInstanceProfileResponse
data InstanceMonitoringState 
data ProductCodesSetItem

data RunningInstance = RunningInstance -- http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-ItemType-RunningInstancesItemType.html
                     { ri_instanceId :: Text
                     , ri_imageId :: Text
                     , ri_instanceState :: InstanceState
                     , ri_privateDnsName :: Text
                     , ri_dnsName :: Text
                     , ri_reason :: Text
                     , ri_keyName :: Text
                     , ri_amiLaunchIndex :: Text
                     -- , ri_productCodes :: ProductCodesSetItem
                     , ri_instanceType :: Text
                     , ri_launchTime :: UTCTime
                     , ri_placement :: PlacementResponse
                     , ri_kernelId :: Text
                     , ri_ramdiskId :: Text
                     , ri_platform :: Text
                     -- , ri_monitoring :: InstanceMonitoringState
                     , ri_subnetId :: Text
                     , ri_vpcId :: Text
                     , ri_privateIpAddress :: Text
                     , ri_ipAddress :: Text
                     , ri_sourceDestCheck :: Bool
                     , ri_groupSet :: [SecurityGroupItem]
                     , ri_stateReason :: StateReason
                     , ri_architecture :: Text     -- i386 | amd64
                     , ri_rootDeviceType :: Text   -- ebs | instance-store
                     , ri_rootDeviceName :: Text
                     , ri_blockDeviceMapping :: [InstanceBlockDeviceMapping]
                     , ri_instanceLifecycle :: Text -- spot | ""
                     , ri_spotInstanceRequestId :: Text
                     , ri_virtualizationType :: Text -- paravirtual | hvm
                     , ri_clientToken :: Text
                     , ri_tagSet :: [ResourceTag]
                     , ri_hypervisor :: Text -- ovm | xen
                     , ri_networkInterfaceSet :: [InstanceNetworkInterface]
                     -- , ri_iamInstanceProfile :: IamInstanceProfileResponse
                     , ri_ebsOptimized :: Bool
                     , ri_sriovNetSupport :: Text -- simple
                     } deriving (Show, Eq, Ord, Generic)

data Instances = Instances [Reservation]
               deriving (Show, Ord, Eq)


{-
path2 cu p1 p2 cons item = build >>= return . cons
  where
    build = sequence $ cu $/ laxElement p1 &/ laxElement p2 &| item

path1 cu p1 cons item = build >>= return . cons
  where
    build = sequence $ cu $/ laxElement p1 &| item

parseInstances :: MonadThrow m => Cu.Cursor -> m Instances
parseInstances cursor = path2 cursor "reservationSet" "item" Instances item
  where
    item cursor = do
      rs_reservationId <- attr "reservationId" cursor
      rs_ownerId <- attr "ownerId" cursor
      rs_requesterId <- maybeAttr "requesterId" cursor
      rs_groups <- path2 cursor "groupSet" "item" id parseSecurityGroupItem
      let rs_instances = []
      return Reservation{..}

maybeAttr name cursor = return $ listToMaybe $ cursor $/ elContent name
attr name cursor = force ("Missing " ++ Text.unpack name) $
            cursor $// elContent name

parseSecurityGroupItem :: MonadThrow m => Cu.Cursor -> m SecurityGroupItem
parseSecurityGroupItem cursor = do
    sg_groupId <- attr "groupId" cursor
    sg_groupName <- attr "groupName" cursor
    return SecurityGroupItem{..}

-}
