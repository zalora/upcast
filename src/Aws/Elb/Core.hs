{-# LANGUAGE OverloadedStrings
           , FlexibleInstances
           , DeriveDataTypeable
           , RecordWildCards
           #-}

module Aws.Elb.Core (
  ELBMetadata
, elbSignQuery
, elbResponseConsumer
, valueConsumer
, defVersion
) where

import qualified Data.ByteString as B

import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTP

import Aws.Core
import Aws.Query

defVersion :: HTTP.QueryItem
defVersion = ("Version", Just "2012-06-01")

elbSignQuery :: HTTP.Query -> QueryAPIConfiguration qt -> SignatureData -> SignedQuery
elbSignQuery query QueryAPIConfiguration{..} sd = querySignQuery query qd sd
  where
    qd = QueryData { qdRegion = qaRegion
                   , qdEndpoint = B.concat ["elasticloadbalancing.", qaRegion, ".amazonaws.com"]
                   , qdService = "elasticloadbalancing"
                   }

type ELBMetadata = QueryMetadata

elbResponseConsumer = queryResponseConsumer
