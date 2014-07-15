{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           #-}

module Aws.Ec2.Commands.DescribeInstanceStatus where

import Aws.Ec2.TH

data DescribeInstanceStatus = DescribeInstanceStatus { dis_instanceIds :: [Text] }
                       deriving (Show)

ec2ValueTransactionDef ''DescribeInstanceStatus 'DescribeInstanceStatus "instanceStatusSet" "InstanceId"
