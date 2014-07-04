{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           #-}

module Aws.Ec2.Commands.DescribeSubnets where

import Aws.Core
import Aws.Ec2.Core
import qualified Aws.Ec2.Core as EC2
import Aws.Ec2.Types

data DescribeSubnets = DescribeSubnets
                       deriving (Show)

instance SignQuery DescribeSubnets where
    type ServiceConfiguration DescribeSubnets = EC2Configuration
    signQuery _ = ec2SignQuery EC2.DescribeSubnets []

instance ResponseConsumer DescribeSubnets Value where
    type ResponseMetadata Value = EC2Metadata
    responseConsumer _ = ec2ResponseConsumer $ valueConsumer "subnetSet" id

instance Transaction DescribeSubnets Value


