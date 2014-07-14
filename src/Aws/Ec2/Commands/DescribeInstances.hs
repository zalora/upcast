{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           #-}

module Aws.Ec2.Commands.DescribeInstances where

import Aws.Ec2.TH

data DescribeInstances = DescribeInstances { di_instanceIds :: [Text] }
                       deriving (Show)

instance SignQuery DescribeInstances where
    type ServiceConfiguration DescribeInstances = EC2Configuration
    signQuery DescribeInstances{..} = ec2SignQuery $
                                                [ ("Action", qArg "DescribeInstances")
                                                , defVersion
                                                ] +++ enumerate "InstanceId" di_instanceIds qArg

ec2ValueTransaction ''DescribeInstances "reservationSet"

