{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           #-}

module Aws.Ec2.Commands.DescribeInstances where

import Aws.Ec2.TH

data DescribeInstances = DescribeInstances
                       deriving (Show)

ec2ValueTransactionDef ''DescribeInstances "DescribeInstances" "reservationSet"
