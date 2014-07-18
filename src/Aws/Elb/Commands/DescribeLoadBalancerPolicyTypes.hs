{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           #-}

module Aws.Elb.Commands.DescribeLoadBalancerPolicyTypes where

import qualified Text.XML.Cursor as Cu
import Text.XML.Cursor (($.//), (&|))
import qualified Data.Vector as V

import Aws.Elb.TH

data DescribeLoadBalancerPolicyTypes = ListLoadBalancerPolicyTypes
                                     | DescribeLoadBalancerPolicyTypes [Text]

instance SignQuery DescribeLoadBalancerPolicyTypes where
    type ServiceConfiguration DescribeLoadBalancerPolicyTypes = QueryAPIConfiguration
    signQuery ListLoadBalancerPolicyTypes = elbSignQuery $ [ ("Action", qArg "DescribeLoadBalancerPolicyTypes")
                                                           , defVersion
                                                           ]
    signQuery (DescribeLoadBalancerPolicyTypes types) = elbSignQuery $ [ ("Action", qArg "DescribeLoadBalancerPolicyTypes")
                                                                       , defVersion
                                                                       ] +++ enumerate "PolicyTypeNames.member" types qArg

instance ResponseConsumer DescribeLoadBalancerPolicyTypes Value where
    type ResponseMetadata Value = QueryMetadata
    responseConsumer ListLoadBalancerPolicyTypes = queryResponseConsumer $ \cu -> do
      let cu' = cu $.// Cu.laxElement "PolicyTypeName" &| (toValue (XMLValueOptions "item") . Cu.node)
      return $ Array $ V.fromList cu'
    responseConsumer (DescribeLoadBalancerPolicyTypes _) = queryResponseConsumer $ valueConsumerOpt (XMLValueOptions "member") "DescribeLoadBalancerPolicyTypesResult" id

instance Transaction DescribeLoadBalancerPolicyTypes Value
