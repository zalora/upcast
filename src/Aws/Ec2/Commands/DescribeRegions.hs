{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           #-}

module Aws.Ec2.Commands.DescribeRegions where

import Aws.Ec2.TH

data DescribeRegions = DescribeRegions
                       deriving (Show)

ec2ValueTransactionDef ''DescribeRegions "DescribeRegions" "regionInfo"
