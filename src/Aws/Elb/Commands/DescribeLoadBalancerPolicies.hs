{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           #-}

module Aws.Elb.Commands.DescribeLoadBalancerPolicies where

import Aws.Elb.TH

data DescribeLoadBalancerPolicies = DescribeLoadBalancerPolicies Text

instance SignQuery DescribeLoadBalancerPolicies where
    type ServiceConfiguration DescribeLoadBalancerPolicies = QueryAPIConfiguration
    signQuery (DescribeLoadBalancerPolicies lb) = elbSignQuery $ [ ("Action", qArg "DescribeLoadBalancerPolicies")
                                                                 , defVersion
                                                                 , ("LoadBalancerName", qArg lb)
                                                                 ]

elbValueTransaction ''DescribeLoadBalancerPolicies "DescribeLoadBalancerPoliciesResult"
