{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.Ec2.Commands.CreateVpc where

import Data.Text (Text, pack, toLower)
import Aws.Ec2.TH

data CreateVpc = CreateVpc
               { cvpc_cidrBlock :: Text
               , cvpc_instanceTenancy :: InstanceTenancy
               } deriving (Show)

instance SignQuery CreateVpc where
    type ServiceConfiguration CreateVpc = EC2Configuration
    signQuery CreateVpc{..} = ec2SignQuery [ ("CidrBlock", qArg cvpc_cidrBlock)
                                           , ("InstanceTenancy", qArg $ toLower $ pack $ show cvpc_instanceTenancy)
                                           , ("Action", qArg "CreateVpc")
                                           , defVersion
                                           ]

ec2ValueTransaction ''CreateVpc "vpc"
