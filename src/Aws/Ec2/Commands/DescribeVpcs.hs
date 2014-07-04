{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           #-}

module Aws.Ec2.Commands.DescribeVpcs where

import Aws.Core
import Aws.Ec2.Core
import qualified Aws.Ec2.Core as EC2
import Aws.Ec2.Types

data DescribeVpcs = DescribeVpcs
                       deriving (Show)

instance SignQuery DescribeVpcs where
    type ServiceConfiguration DescribeVpcs = EC2Configuration
    signQuery _ = ec2SignQuery EC2.DescribeVpcs []

instance ResponseConsumer DescribeVpcs Value where
    type ResponseMetadata Value = EC2Metadata
    responseConsumer _ = ec2ResponseConsumer $ valueConsumer "vpcSet" id

instance Transaction DescribeVpcs Value


