{-# LANGUAGE OverloadedStrings
           , FlexibleInstances
           , DeriveDataTypeable
           , RecordWildCards
           #-}

module Aws.Ec2.Core (
  EC2Configuration
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

type EC2Metadata = QueryMetadata
type EC2Configuration = QueryAPIConfiguration

defVersion :: HTTP.QueryItem
defVersion = ("Version", Just "2014-06-15")

ec2SignQuery :: HTTP.Query -> QueryAPIConfiguration qt -> SignatureData -> SignedQuery
ec2SignQuery query QueryAPIConfiguration{..} sd = querySignQuery query qd sd
  where
    qd = QueryData { qdRegion = qaRegion
                   , qdEndpoint = B.concat ["ec2.", qaRegion, ".amazonaws.com"]
                   , qdService = "ec2"
                   }

ec2ResponseConsumer = queryResponseConsumer
