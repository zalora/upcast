{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           #-}

module Aws.Elb.Commands.CreateLoadBalancer where

import qualified Network.HTTP.Types as HTTP
import Aws.Elb.TH

data LbProtocol = HTTP | HTTPS | TCP | SSL
                deriving (Show)

data Scheme = Internal | Public
            deriving (Show)

data Listener = Listener
              { l_lbPort :: Int
              , l_instancePort :: Int
              , l_instanceProtocol :: LbProtocol
              , l_lbProtocol :: LbProtocol
              , l_sslCertificateId :: Maybe Text
              } deriving (Show)

data CreateLoadBalancer = CreateLoadBalancer
                        { clb_name :: Text
                        , clb_listeners :: [Listener]
                        , clb_scheme :: Scheme
                        , clb_securityGroupIds :: [Text]
                        , clb_subnetIds :: [Text] -- ^ one per AZ
                        } deriving (Show)

enumerateListeners :: [Listener] -> HTTP.Query
enumerateListeners = enumerateLists "Listeners.member." . fmap unroll
  where
    unroll Listener{..} = [ ("LoadBalancerPort", qShow l_lbPort)
                          , ("InstancePort", qShow l_instancePort)
                          , ("InstanceProtocol", qShow l_instanceProtocol)
                          , ("Protocol", qShow l_lbProtocol)
                          ] +++ optionalA "SSLCertificateId" l_sslCertificateId

instance SignQuery CreateLoadBalancer where
    type ServiceConfiguration CreateLoadBalancer = QueryAPIConfiguration
    signQuery CreateLoadBalancer{..} = elbSignQuery $
                                                    [ ("Action", qArg "CreateLoadBalancer")
                                                    , defVersion
                                                    , ("LoadBalancerName", qArg clb_name)
                                                    ] +++ enumerateListeners clb_listeners
                                                      +++ enumerate "Subnets.member" clb_subnetIds qArg
                                                      +++ enumerate "SecurityGroups.member" clb_securityGroupIds qArg
                                                      +++ case clb_scheme of
                                                            Internal -> [("Scheme", qArg "internal")]
                                                            _ -> []


elbValueTransaction ''CreateLoadBalancer "CreateLoadBalancerResult"
