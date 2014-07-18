{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           #-}

module Aws.Elb.Commands.RegisterInstancesWithLoadBalancer where

import qualified Network.HTTP.Types as HTTP
import Aws.Elb.TH

data RegisterInstancesWithLoadBalancer = RegisterInstancesWithLoadBalancer
                        { rilb_name :: Text
                        , rilb_instanceIds :: [Text]
                        } deriving (Show)

enumerateInstanceIds :: [Text] -> HTTP.Query
enumerateInstanceIds = enumerateLists "Instances.member." . fmap unroll
  where
    unroll i = [("InstanceId", qArg i)]

instance SignQuery RegisterInstancesWithLoadBalancer where
    type ServiceConfiguration RegisterInstancesWithLoadBalancer = QueryAPIConfiguration
    signQuery RegisterInstancesWithLoadBalancer{..} = elbSignQuery $
                                                    [ ("Action", qArg "RegisterInstancesWithLoadBalancer")
                                                    , defVersion
                                                    , ("LoadBalancerName", qArg rilb_name)
                                                    ] +++ enumerateInstanceIds rilb_instanceIds

elbValueTransaction ''RegisterInstancesWithLoadBalancer "RegisterInstancesWithLoadBalancerResult"
