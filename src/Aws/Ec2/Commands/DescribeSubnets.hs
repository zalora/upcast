{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           #-}

module Aws.Ec2.Commands.DescribeSubnets where

import Aws.Ec2.TH

data DescribeSubnets = DescribeSubnets
                       deriving (Show)

ec2ValueTransactionDef ''DescribeSubnets "DescribeSubnets" "subnetSet" 
