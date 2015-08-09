{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- Amazonka 0.3.6.1 is improperly generating XML in at least amazonka-route53 for:
-- * lists of things (double-encodes list headers)
-- * namespace inheritance (see `element' vs original `element')
-- * xmlns uses http instead of https
--
-- This is a specialized variant of ChangeResourceRecordSets for alias upserts.
--
-- This may over-specialize things but I was changing code until I found that
-- xmlns was different from what AWS docs suggest.

module Upcast.Infra.Amazonka.UpsertAlias where

import qualified GHC.Exts
import           Network.AWS.Prelude hiding ((=@), element, nodes)
import           Network.AWS.Request.RestXML
import           Network.AWS.Route53.ChangeResourceRecordSets
import           Network.AWS.Route53.Types hiding (ns)
import           Text.XML

data UpsertAlias = UpsertAlias { uaHostedZoneId :: Text
                               , uaName :: Text
                               , uaRecordType :: RecordType
                               , uaTarget :: AliasTarget
                               }

-- https here is important:
ns = "https://route53.amazonaws.com/doc/2013-04-01/"

-- These functions force namespace on every element:
element n = fromJust . namespaced ns n

nodes :: Text -> [Node] -> [Node]
nodes n nodes' = [NodeElement (element n nodes')]

(=@) :: Text -> [Node] -> Node
n =@ x = NodeElement (element n x)

uaChangeBatch UpsertAlias{..} = "ChangeBatch" =@ ["Changes" =@ [change]]
  where
    aliasTarget =
      [ "DNSName" =@ toXMLText (uaTarget ^. atDNSName)
      , "EvaluateTargetHealth" =@ toXMLText (uaTarget ^. atEvaluateTargetHealth)
      , "HostedZoneId" =@ toXMLText (uaTarget ^. atHostedZoneId)
      ]
    change = "Change" =@
      [ "Action" =@ toXMLText Upsert
      , "ResourceRecordSet" =@ [ "Name" =@ toXMLText uaName
                               , "Type" =@ toXMLText uaRecordType
                               , "AliasTarget" =@ aliasTarget
                               ]
      ]

instance ToPath UpsertAlias where
  toPath UpsertAlias{..} = mconcat
                           [ "/2013-04-01/hostedzone/"
                           , toText uaHostedZoneId
                           , "/rrset/"
                           ]

instance ToQuery UpsertAlias where
  toQuery = const mempty

instance ToHeaders UpsertAlias

instance ToXMLRoot UpsertAlias where
  toXMLRoot ua = Just $ element "ChangeResourceRecordSetsRequest" [ uaChangeBatch ua ]

instance ToXML UpsertAlias

instance AWSRequest UpsertAlias where
  type Sv UpsertAlias = Route53
  type Rs UpsertAlias = ChangeResourceRecordSetsResponse

  request  = post
  response = xmlResponse
