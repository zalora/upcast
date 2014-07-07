{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           , RecordWildCards
           #-}

module Aws.Ec2.Commands.DescribeImages where

import Aws.Ec2.TH

data DescribeImages = DescribeImages
                       deriving (Show)

instance SignQuery DescribeImages where
    type ServiceConfiguration DescribeImages = EC2Configuration
    signQuery DescribeImages{..} = ec2SignQuery [ ("Action", qArg "DescribeImages")
                                                , defVersion
                                                , ("Owner.1", qArg "self") -- XXX
                                                ]

ec2ValueTransaction ''DescribeImages "imagesSet"
