{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           #-}

module Aws.Ec2.Commands.CreateSubnet where

import Data.Text (Text)

import Aws.Core
import Aws.Ec2.Core hiding (EC2Request(..))
import qualified Aws.Ec2.Core as EC2
import Aws.Ec2.Types

data CreateSubnet = CreateSubnet
               { csub_vpcId :: Text
               , csub_cidrBlock :: Text
               } deriving (Show)

instance SignQuery CreateSubnet where
    type ServiceConfiguration CreateSubnet = EC2Configuration
    signQuery CreateSubnet{..} = ec2SignQuery EC2.CreateSubnet [ ("VpcId", qArg csub_vpcId)
                                                               , ("CidrBlock", qArg csub_cidrBlock)
                                                               ]

instance ResponseConsumer CreateSubnet Value where
    type ResponseMetadata Value = EC2Metadata
    responseConsumer _ = ec2ResponseConsumer $ valueConsumer "subnet" id

instance Transaction CreateSubnet Value

