{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           , LambdaCase
           #-}

module Aws.Ec2.Commands.RunInstances where

import Data.Text (Text)
import Data.ByteString.Char8 (pack, ByteString)
import qualified Network.HTTP.Types as HTTP
import Data.Monoid
import Aws.Ec2.TH

-- http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RunInstances.html
data RunInstances = RunInstances
                  { run_imageId :: Text
                  , run_count :: (Int, Int)
                  , run_instanceType :: Text
                  , run_securityGroupIds :: [Text]
                  , run_blockDeviceMappings :: [BlockDeviceMapping]
                  , run_subnetId :: Text
                  , run_monitoringEnabled :: Bool
                  , run_disableApiTermination :: Bool
                  , run_instanceInitiatedShutdownBehavior :: InstanceInitiatedShutdownBehavior
                  , run_ebsOptimized :: Bool

                  , run_keyName :: Maybe Text
                  , run_userData :: Maybe Text -- Base64-encoded
                  , run_kernelId :: Maybe Text
                  , run_ramdiskId :: Maybe Text
                  , run_clientToken :: Maybe Text
                  -- , run_placement :: Maybe Placement
                  -- missing: NetworkInterface
                  } deriving (Show)

data InstanceInitiatedShutdownBehavior = Stop | Terminate

instance Show InstanceInitiatedShutdownBehavior where
    show Stop = "stop"
    show Terminate = "terminate"

data Placement = Placement
               { pla_availabilityZone :: Text
               , pla_groupName :: Text
               , pla_tenancy :: InstanceTenancy
               } deriving (Show)

enumerateBlockDevices :: [BlockDeviceMapping] -> HTTP.Query
enumerateBlockDevices = enumerateLists "BlockDeviceMapping." . fmap unroll
  where
    unroll BlockDeviceMapping{..} = [ ("DeviceName", qArg bdm_deviceName)
                                    ] +++ case bdm_device of
                                            Ephemeral{..} -> [("VirtualName", qArg bdm_virtualName)]
                                            EBS ebs -> queryEbsBlockDevice ebs                                            

instance SignQuery RunInstances where
    type ServiceConfiguration RunInstances = EC2Configuration
    signQuery RunInstances{..} = ec2SignQuery $
                                  main
                                  +++ (optionalA "KeyName" run_keyName)
                                  +++ (optionalA "UserData" run_userData)
                                  +++ (optionalA "KernelId" run_kernelId)
                                  +++ (optionalA "RamdiskId" run_ramdiskId)
                                  +++ (optionalA "ClientToken" run_clientToken)
                                  +++ enumerate "NetworkInterface.0.SecurityGroupId" run_securityGroupIds qArg
                                  +++ enumerateBlockDevices run_blockDeviceMappings
        where
          main :: HTTP.Query
          main = [ ("Action", qArg "RunInstances")
                 , defVersion
                 , ("ImageId", qArg run_imageId)
                 , ("MinCount", qShow $ fst run_count)
                 , ("MaxCount", qShow $ snd run_count)
                 , ("InstanceType", qArg run_instanceType)
                 , ("Monitoring.Enabled", qShow run_monitoringEnabled)
                 , ("DisableApiTermination", qShow run_disableApiTermination)
                 , ("InstanceInitiatedShutdownBehavior", qShow run_instanceInitiatedShutdownBehavior)
                 , ("EbsOptimized", qShow run_ebsOptimized)
                 -- cheating:
                 , ("NetworkInterface.0.DeviceIndex", qShow 0)
                 , ("NetworkInterface.0.SubnetId", qArg run_subnetId)
                 , ("NetworkInterface.0.AssociatePublicIpAddress", qShow True) -- default is False if subnets are present
                 ]

ec2ValueTransaction ''RunInstances "RunInstancesResponse"

