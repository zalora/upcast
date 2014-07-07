{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.Ec2.Commands.CreateVolume where

import Data.Text (Text)
import Data.Monoid
import Aws.Ec2.TH

data CreateVolume = CreateVolume
               { cvol_gbSize :: Text
               , cvol_SnapshotId :: Maybe Text
               , cvol_AvailabilityZone :: Text
               , cvol_VolumeType :: VolumeType
               , cvol_Encrypted :: Bool
               } deriving (Show)

data VolumeType = Standard | GP2SSD | IOPSSD Int

instance Show VolumeType where
    show Standard = "standard"
    show GP2SSD = "gp2"
    show (IOPSSD _) = "io1"

instance SignQuery CreateVolume where
    type ServiceConfiguration CreateVolume = EC2Configuration
    signQuery CreateVolume{..} = ec2SignQuery $ [ ("Action", qArg "CreateVolume")
                                              , defVersion
                                              , ("Size", qArg cvol_gbSize)
                                              , ("VolumeType", qShow cvol_VolumeType)
                                              , ("Encrypted", qShow cvol_Encrypted)
                                              ] `mappend` (
                                                          case cvol_VolumeType of
                                                              IOPSSD iops -> [("Iops", qShow iops)]
                                                              _ -> []
                                              ) `mappend` (
                                                          case cvol_SnapshotId of
                                                            Nothing -> []
                                                            Just t -> [("SnapshotId", qArg t)]
                                              )

ec2ValueTransaction ''CreateVolume "CreateVolumeResponse"
