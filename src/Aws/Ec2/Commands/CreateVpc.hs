{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           #-}

module Aws.Ec2.Commands.CreateVpc where

import Data.Text (Text, pack, toLower)

import Aws.Core
import Aws.Ec2.Core hiding (CreateVpc)
import qualified Aws.Ec2.Core as EC2
import Aws.Ec2.Types

data CreateVpc = CreateVpc
               { cvpc_cidrBlock :: Text
               , cvpc_instanceTenancy :: InstanceTenancy
               } deriving (Show)

data InstanceTenancy = Default | Dedicated
                     deriving (Show)

instance SignQuery CreateVpc where
    type ServiceConfiguration CreateVpc = EC2Configuration
    signQuery CreateVpc{..} = ec2SignQuery EC2.CreateVpc [ ("CidrBlock", qArg cvpc_cidrBlock)
                                                         , ("InstanceTenancy", qArg $ toLower $ pack $ show cvpc_instanceTenancy)
                                                         ]

instance ResponseConsumer CreateVpc Value where
    type ResponseMetadata Value = EC2Metadata
    responseConsumer _ = ec2ResponseConsumer $ valueConsumer "vpc" id

instance Transaction CreateVpc Value
