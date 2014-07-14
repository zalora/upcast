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

instance SignQuery DescribeInstanceStatus where
    type ServiceConfiguration DescribeInstanceStatus = EC2Configuration
    signQuery DescribeInstanceStatus{..} = ec2SignQuery $
                                                [ ("Action", qArg "DescribeInstanceStatus")
                                                , defVersion
                                                ] +++ enumerate "InstanceId" dis_instanceIds qArg

ec2ValueTransaction ''DescribeInstanceStatus "instanceStatusSet"

