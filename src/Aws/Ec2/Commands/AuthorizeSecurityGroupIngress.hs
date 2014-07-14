{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.Ec2.Commands.AuthorizeSecurityGroupIngress where

import qualified Network.HTTP.Types as HTTP
import Data.Monoid (mconcat)
import Data.Text (Text)
import Data.ByteString.Char8 (pack, ByteString)
import Aws.Ec2.TH

data IpProtocol = TCP | UDP | ICMP | Proto Int | All

instance Show IpProtocol where
    show TCP = "tcp" -- 6
    show UDP = "udp" -- 17
    show ICMP = "icmp" -- 1
    show (Proto i) = show i
    show All = "-1"

type CidrIp = Text

data IpPermission = IpPermission IpProtocol (Maybe Int) (Maybe Int) [CidrIp]
                deriving (Show)

data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress
               { asi_groupId :: Text
               , asi_permissions :: [IpPermission]
               } deriving (Show)

enumeratePermissions :: [IpPermission] -> HTTP.Query
enumeratePermissions = enumerateLists "IpPermissions." . fmap unroll
  where
    unroll (IpPermission proto from to ips) = [ ("IpProtocol", qShow proto)
                                              , ("FromPort", qShow $ maybe (-1) id from)
                                              , ("ToPort", qShow $ maybe (-1) id to)
                                              ] +++ [(mconcat [k, ".CidrIp"], v)| (k, v) <- enumerate "IpRanges" ips qArg]


instance SignQuery AuthorizeSecurityGroupIngress where
    type ServiceConfiguration AuthorizeSecurityGroupIngress = EC2Configuration
    signQuery AuthorizeSecurityGroupIngress{..} = ec2SignQuery $
                                           [ ("GroupId", qArg asi_groupId)
                                           , ("Action", qArg "AuthorizeSecurityGroupIngress")
                                           , defVersion
                                           ] +++ enumeratePermissions asi_permissions

ec2ValueTransaction ''AuthorizeSecurityGroupIngress "AuthorizeSecurityGroupIngressResponse"
