{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           #-}

module Aws.Elb.Commands.RegisterInstancesWithLoadBalancer where

import Aws.Elb.TH

data RegisterInstancesWithLoadBalancer = RegisterInstancesWithLoadBalancer
                        { rilb_name :: Text
                        , rilb_instanceIds :: [Text]
                        } deriving (Show)

instance SignQuery RegisterInstancesWithLoadBalancer where
    type ServiceConfiguration RegisterInstancesWithLoadBalancer = QueryAPIConfiguration
    signQuery RegisterInstancesWithLoadBalancer{..} = elbSignQuery $
                                                    [ ("Action", qArg "RegisterInstancesWithLoadBalancer")
                                                    , defVersion
                                                    , ("LoadBalancerName", qArg rilb_name)
                                                    ] +++ enumerate "Instances.member" rilb_instanceIds qArg

elbValueTransaction ''RegisterInstancesWithLoadBalancer "RegisterInstancesWithLoadBalancerResult"
