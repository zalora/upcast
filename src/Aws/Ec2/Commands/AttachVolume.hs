{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.Ec2.Commands.AttachVolume where

import Data.Text (Text)
import Data.Monoid
import Aws.Ec2.TH

data AttachVolume = AttachVolume
               { avol_VolumeId :: Text
               , avol_InstanceId :: Text
               , avol_Device :: Text
               } deriving (Show)

instance SignQuery AttachVolume where
    type ServiceConfiguration AttachVolume = EC2Configuration
    signQuery AttachVolume{..} = ec2SignQuery $ [ ("Action", qArg "AttachVolume")
                                              , defVersion
                                              , ("VolumeId", qArg avol_VolumeId)
                                              , ("InstanceId", qArg avol_InstanceId)
                                              , ("Device", qArg avol_Device)
                                              ]

ec2ValueTransaction ''AttachVolume "AttachVolumeResponse"
