{-# LANGUAGE OverloadedStrings
           , FlexibleInstances
           , DeriveDataTypeable
           , RecordWildCards
           #-}

module Aws.Ec2.Core (
  EC2Configuration(..)
, EC2Metadata
, ec2SignQuery
, ec2ResponseConsumer
, valueConsumer
, defVersion
) where

import qualified Data.ByteString as B

import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTP

import Aws.Core
import Aws.Query

data EC2Configuration qt = EC2Configuration
                         { ec2Region :: B.ByteString
                         } deriving (Show)

instance DefaultServiceConfiguration (EC2Configuration NormalQuery) where
  defServiceConfig = EC2Configuration "us-east-1"
  debugServiceConfig = EC2Configuration "us-east-1"

defVersion :: HTTP.QueryItem
defVersion = ("Version", Just "2014-06-15")

ec2SignQuery :: HTTP.Query -> EC2Configuration qt -> SignatureData -> SignedQuery
ec2SignQuery query EC2Configuration{..} sd = querySignQuery query qd sd
  where
    qd = QueryData { qdRegion = ec2Region
                   , qdEndpoint = B.concat ["ec2.", ec2Region, ".amazonaws.com"]
                   , qdService = "ec2"
                   }

type EC2Metadata = QueryMetadata

ec2ResponseConsumer = queryResponseConsumer
