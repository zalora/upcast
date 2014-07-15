{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.Ec2.Commands.AttachInternetGateway where

import Data.Text (Text, pack, toLower)
import Aws.Ec2.TH

data AttachInternetGateway = AttachInternetGateway
               { aig_internetGatewayId :: Text
               , aig_vpcId :: Text
               } deriving (Show)

instance SignQuery AttachInternetGateway where
    type ServiceConfiguration AttachInternetGateway = EC2Configuration
    signQuery AttachInternetGateway{..} = ec2SignQuery  $
                                           [ ("InternetGatewayId", qArg aig_internetGatewayId)
                                           , ("VpcId", qArg aig_vpcId)
                                           , ("Action", qArg "AttachInternetGateway")
                                           , defVersion
                                           ]

ec2ValueTransaction ''AttachInternetGateway "return"
