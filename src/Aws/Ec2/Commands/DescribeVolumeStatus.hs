{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           #-}

module Aws.Ec2.Commands.DescribeVolumeStatus where

import Aws.Ec2.TH

data DescribeVolumeStatus = DescribeVolumeStatus
                       deriving (Show)

ec2ValueTransactionDef ''DescribeVolumeStatus "DescribeVolumeStatus" "volumeStatusSet"
