{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.Ec2.Commands.CreateSubnet where

import Data.Text (Text)
import Aws.Ec2.TH

data CreateSubnet = CreateSubnet
               { csub_vpcId :: Text
               , csub_cidrBlock :: Text
               , csub_availabilityZone :: Maybe Text
               } deriving (Show)

instance SignQuery CreateSubnet where
    type ServiceConfiguration CreateSubnet = EC2Configuration
    signQuery CreateSubnet{..} = ec2SignQuery $
                                              [ ("Action", qArg "CreateSubnet")
                                              , defVersion
                                              , ("VpcId", qArg csub_vpcId)
                                              , ("CidrBlock", qArg csub_cidrBlock)
                                              ] +++ (optionalA "AvailabilityZone" csub_availabilityZone)

ec2ValueTransaction ''CreateSubnet "subnet"
