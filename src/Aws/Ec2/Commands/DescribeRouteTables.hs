{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , TemplateHaskell
           , RecordWildCards
           #-}

-- | Optimized to lookup a default route table for VPC.

module Aws.Ec2.Commands.DescribeRouteTables where

import Aws.Ec2.TH

data DescribeRouteTables = DescribeRouteTables { drt_vpcId :: Text }
                       deriving (Show)

instance SignQuery DescribeRouteTables where
    type ServiceConfiguration DescribeRouteTables = EC2Configuration
    signQuery DescribeRouteTables{..} = ec2SignQuery $
                                           [ ("Action", qArg "DescribeRouteTables")
                                           , defVersion
                                           , ("Filter.1.Name", qArg "vpc-id")
                                           , ("Filter.1.Value.1", qArg drt_vpcId)
                                           ]

ec2ValueTransaction ''DescribeRouteTables "routeTableSet"
