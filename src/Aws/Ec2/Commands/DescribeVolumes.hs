{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           #-}

module Aws.Ec2.Commands.DescribeVolumes where

import Aws.Ec2.TH

data DescribeVolumes = DescribeVolumes
                       deriving (Show)

ec2ValueTransactionDef ''DescribeVolumes "DescribeVolumes" "volumeSet"
