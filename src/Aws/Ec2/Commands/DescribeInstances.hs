{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           #-}

module Aws.Ec2.Commands.DescribeInstances where

import Aws.Core
import Aws.Ec2.Core
import qualified Aws.Ec2.Core as EC2
import Aws.Ec2.Types

data DescribeInstances = DescribeInstances
                       deriving (Show)

instance SignQuery DescribeInstances where
    type ServiceConfiguration DescribeInstances = EC2Configuration
    signQuery _ = ec2SignQuery EC2.DescribeInstances []

instance ResponseConsumer DescribeInstances Value where
    type ResponseMetadata Value = EC2Metadata
    responseConsumer _ = ec2ResponseConsumer $ valueConsumer "reservationSet" id

instance Transaction DescribeInstances Value

