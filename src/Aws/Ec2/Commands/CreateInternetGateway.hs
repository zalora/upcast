{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.Ec2.Commands.CreateInternetGateway where

import Aws.Ec2.TH

data CreateInternetGateway = CreateInternetGateway
                           deriving (Show)

instance SignQuery CreateInternetGateway where
    type ServiceConfiguration CreateInternetGateway = EC2Configuration
    signQuery CreateInternetGateway{..} = ec2SignQuery $ 
                                           [ ("Action", qArg "CreateInternetGateway")
                                           , defVersion
                                           ]

ec2ValueTransaction ''CreateInternetGateway "internetGateway"
