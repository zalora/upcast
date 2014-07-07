{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , TemplateHaskell
           #-}

module Aws.Ec2.Commands.DescribeVpcs where

import Aws.Ec2.TH

data DescribeVpcs = DescribeVpcs
                       deriving (Show)

ec2ValueTransactionDef ''DescribeVpcs "DescribeVpcs" "vpcSet" 
