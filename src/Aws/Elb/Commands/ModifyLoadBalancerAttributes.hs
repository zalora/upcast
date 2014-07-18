{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           #-}

module Aws.Elb.Commands.ModifyLoadBalancerAttributes where

import Aws.Elb.TH

type S3BucketName = Text
type S3BucketPrefix = Text
data EmitInterval = Min5 | Min60 deriving (Show)

data LoadBalancerAttribute = AccessLog Bool EmitInterval S3BucketName S3BucketPrefix
                           | ConnectionDraining Bool Int
                           | CrossZoneLoadBalancing Bool
                           deriving (Show)

data ModifyLoadBalancerAttributes = ModifyLoadBalancerAttributes
                        { mlba_name :: Text
                        , mlba_attributes :: [LoadBalancerAttribute]
                        } deriving (Show)

attributeQuery (AccessLog enabled interval bucket prefix)
      = [ ("LoadBalancerAttributes.AccessLog.Enabled", qShow enabled)
        , ("LoadBalancerAttributes.AccessLog.EmitInterval", int interval)
        , ("LoadBalancerAttributes.AccessLog.S3BucketName", qArg bucket)
        , ("LoadBalancerAttributes.AccessLog.S3BucketPrefix", qArg prefix)
        ] where int Min5 = qShow 5
                int Min60 = qShow 60
attributeQuery (ConnectionDraining enabled timeout)
      = [ ("LoadBalancerAttributes.ConnectionDraining.Enabled", qShow enabled)
        , ("LoadBalancerAttributes.ConnectionDraining.Timeout", qShow timeout)
        ]
attributeQuery (CrossZoneLoadBalancing enabled)
      = [ ("LoadBalancerAttributes.CrossZoneLoadBalancing.Enabled", qShow enabled)
        ]

instance SignQuery ModifyLoadBalancerAttributes where
    type ServiceConfiguration ModifyLoadBalancerAttributes = QueryAPIConfiguration
    signQuery ModifyLoadBalancerAttributes{..} = elbSignQuery $
                                                    [ ("Action", qArg "ModifyLoadBalancerAttributes")
                                                    , defVersion
                                                    , ("LoadBalancerName", qArg mlba_name)
                                                    ] +++ concatMap attributeQuery mlba_attributes

elbValueTransaction ''ModifyLoadBalancerAttributes "ModifyLoadBalancerAttributesResult"
