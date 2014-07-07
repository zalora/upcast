{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , RecordWildCards
           , LambdaCase
           #-}

module Aws.Ec2.Info where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Monoid
import Data.ByteString.Char8 (ByteString, pack)

import Network.HTTP.Types as HTTP

import Aws.Ec2.Core

data InstanceTenancy = Default | Dedicated

instance Show InstanceTenancy where
    show Default = "default"
    show Dedicated = "dedicated"

data VolumeType = Standard | GP2SSD | IOPSSD Int

instance Show VolumeType where
    show Standard = "standard"
    show GP2SSD = "gp2"
    show (IOPSSD _) = "io1"

data BlockDeviceMapping = BlockDeviceMapping
                                { bdm_deviceName :: Text
                                , bdm_device :: BlockDevice
                                } deriving (Show)

data BlockDevice = Ephemeral {bdm_virtualName :: Text}
                 | EBS EbsBlockDevice
                 deriving (Show)

data EbsBlockDevice = EbsBlockDevice
                    { ebd_snapshotId :: Maybe Text
                    , ebd_deleteOnTermination :: Bool
                    , ebd_volumeType :: VolumeType
                    , ebd_volumeSize :: Int
                    , ebd_encrypted :: Bool
                    } deriving (Show)

queryEbsBlockDevice EbsBlockDevice{..} = [ ("VolumeType", qShow ebd_volumeType)
                                         , ("VolumeSize", qShow ebd_volumeSize)
                                         , ("DeleteOnTermination", qShow ebd_deleteOnTermination)
                                         , ("Encrypted", qShow ebd_encrypted)
                                         ] +++ optionalA "SnapshotId" ebd_snapshotId
                                           +++ case ebd_volumeType of
                                                 IOPSSD iops -> [("Iops", qShow iops)]
                                                 _ -> []


-- http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RunInstances.html
instanceTypes :: [Text]
instanceTypes = [ "t2.micro"
                , "t2.small"
                , "t2.medium"
                , "m3.medium"
                , "m3.large"
                , "m3.xlarge"
                , "m3.2xlarge"
                , "m1.small" -- default
                , "m1.medium"
                , "m1.large"
                , "m1.xlarge"
                , "c3.large"
                , "c3.xlarge"
                , "c3.2xlarge"
                , "c3.4xlarge"
                , "c3.8xlarge"
                , "c1.medium"
                , "c1.xlarge"
                , "cc2.8xlarge"
                , "r3.large"
                , "r3.xlarge"
                , "r3.2xlarge"
                , "r3.4xlarge"
                , "r3.8xlarge"
                , "m2.xlarge"
                , "m2.2xlarge"
                , "m2.4xlarge"
                , "cr1.8xlarge"
                , "i2.xlarge"
                , "i2.2xlarge"
                , "i2.4xlarge"
                , "i2.8xlarge"
                , "hs1.8xlarge"
                , "hi1.4xlarge"
                , "t1.micro"
                , "g2.2xlarge"
                , "cg1.4xlarge"
                ]


(+++) = mappend

optional :: ByteString -> Maybe a -> (a -> Maybe ByteString) -> HTTP.Query
optional k (Just x) f = [(k, f x)]
optional k Nothing f = []

optionalA k v = optional k v qArg

enumerate :: String -> [a] -> (a -> Maybe ByteString) -> HTTP.Query
enumerate k xs f = [(pack $ mconcat [k, ".", show n], f x) | (n, x) <- zip ([1..] :: [Int]) xs]

