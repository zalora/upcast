{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           #-}

module Aws.Ec2.Commands.DescribeAvailabilityZones where

import Aws.Ec2.TH

data DescribeAvailabilityZones = DescribeAvailabilityZones [Text]
                       deriving (Show)

ec2ValueTransactionDef ''DescribeAvailabilityZones 'DescribeAvailabilityZones "availabilityZoneInfo" "ZoneName"
