{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           #-}

module Aws.Ec2.Commands.DescribeInstances where

import qualified Text.XML.Cursor as Cu
import Text.XML.Cursor (($.//))

import Aws.Core
import Aws.Ec2.Core
import qualified Aws.Ec2.Core as EC2
import Aws.Ec2.Types

data DescribeInstances = DescribeInstances
                       deriving (Show)

data DescribeInstancesResponse = DescribeInstancesResponse Value
                               deriving (Show)

instance SignQuery DescribeInstances where
    type ServiceConfiguration DescribeInstances = EC2Configuration
    signQuery = ec2SignQuery EC2.DescribeInstances

instance ResponseConsumer DescribeInstances DescribeInstancesResponse where
    type ResponseMetadata DescribeInstancesResponse = EC2Metadata
    responseConsumer _ = ec2ResponseConsumer $ \cu -> do
      let cu' = cu $.// Cu.laxElement "reservationSet"
      let f = return . DescribeInstancesResponse . toValue . Cu.node 
      f $ head cu'

instance AsMemoryResponse DescribeInstancesResponse where
    type MemoryResponse DescribeInstancesResponse = DescribeInstancesResponse
    loadToMemory = return

instance Transaction DescribeInstances DescribeInstancesResponse



