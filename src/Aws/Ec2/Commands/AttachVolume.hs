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
               { avol_volumeId :: Text
               , avol_instanceId :: Text
               , avol_device :: Text
               } deriving (Show)

instance SignQuery AttachVolume where
    type ServiceConfiguration AttachVolume = EC2Configuration
    signQuery AttachVolume{..} = ec2SignQuery $ [ ("Action", qArg "AttachVolume")
                                              , defVersion
                                              , ("VolumeId", qArg avol_volumeId)
                                              , ("InstanceId", qArg avol_instanceId)
                                              , ("Device", qArg avol_device)
                                              ]

ec2ValueTransaction ''AttachVolume "AttachVolumeResponse"
