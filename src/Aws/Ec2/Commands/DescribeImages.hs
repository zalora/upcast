{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           , RecordWildCards
           #-}

module Aws.Ec2.Commands.DescribeImages where

import Aws.Ec2.TH

data DescribeImages = DescribeImages { di_imageIds :: [Text] }
                      deriving (Show)

instance SignQuery DescribeImages where
    type ServiceConfiguration DescribeImages = EC2Configuration
    signQuery DescribeImages{..} = ec2SignQuery $
                                                [ ("Action", qArg "DescribeImages")
                                                , defVersion
                                                ] +++ case di_imageIds of
                                                        [] -> [("Owner.1", qArg "self")]
                                                        _ -> enumerate "ImageId" di_imageIds qArg

ec2ValueTransaction ''DescribeImages "imagesSet"
