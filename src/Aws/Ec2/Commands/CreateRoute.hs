{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , TemplateHaskell
           , RecordWildCards
           #-}

-- | Optimized to lookup a default route table for VPC.

module Aws.Ec2.Commands.CreateRoute where

import Aws.Ec2.TH

data RouteTarget = GatewayId Text
                 | InstanceId Text
                 | NetworkInterfaceId Text
                 | VpcPeeringConnectionId Text
                 deriving (Show)

data CreateRoute = CreateRoute
                 { cr_routeTableId :: Text
                 , cr_destinationCidrBlock :: Text
                 , cr_target :: RouteTarget
                 } deriving (Show)

instance SignQuery CreateRoute where
    type ServiceConfiguration CreateRoute = EC2Configuration
    signQuery CreateRoute{..} = ec2SignQuery $
                                           [ ("Action", qArg "CreateRoute")
                                           , defVersion
                                           , ("RouteTableId", qArg cr_routeTableId)
                                           , ("DestinationCidrBlock", qArg cr_destinationCidrBlock)
                                           ] +++ case cr_target of
                                                   GatewayId t -> [("GatewayId", qArg t)]
                                                   InstanceId t -> [("InstanceId", qArg t)]
                                                   NetworkInterfaceId t -> [("NetworkInterfaceId", qArg t)]
                                                   VpcPeeringConnectionId t -> [("VpcPeeringConnectionId", qArg t)]

ec2ValueTransaction ''CreateRoute "return"
