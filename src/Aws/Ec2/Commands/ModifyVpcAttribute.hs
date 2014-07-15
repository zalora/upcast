{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.Ec2.Commands.ModifyVpcAttribute where

import Aws.Ec2.TH

data VpcAttribute = EnableDnsSupport Bool | EnableDnsHostnames Bool
                  deriving (Show)

data ModifyVpcAttribute = ModifyVpcAttribute
               { mva_vpcId :: Text
               , mva_attribute :: VpcAttribute
               } deriving (Show)

instance SignQuery ModifyVpcAttribute where
    type ServiceConfiguration ModifyVpcAttribute = EC2Configuration
    signQuery ModifyVpcAttribute{..} = ec2SignQuery $
                                           [ ("VpcId", qArg mva_vpcId)
                                           , ("Action", qArg "ModifyVpcAttribute")
                                           , defVersion
                                           ] +++ case mva_attribute of
                                                   EnableDnsSupport es -> [("EnableDnsSupport.Value", qShow es)]
                                                   EnableDnsHostnames eh -> [("EnableDnsHostnames.Value", qShow eh)]

ec2ValueTransaction ''ModifyVpcAttribute "return"
