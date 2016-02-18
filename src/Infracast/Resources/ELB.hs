{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Infracast.Resources.ELB where

import           Control.Applicative -- (*)
import           Control.Lens hiding (Context) -- (*)
import           Control.Lens.Action -- (*)
import           Control.Monad (void, unless)
import           Control.Monad.Reader (asks)
import           Control.Monad.Trans.AWS (Rs, AWSPager(..), send, await, paginate, AWST, runAWST)
import           Data.Hashable (Hashable(..))
import           Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import           Data.Map (Map)
import           Data.Maybe (fromMaybe, maybeToList, fromJust, isNothing)
import           Data.Monoid ((<>))
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T (isPrefixOf, take)
import           Data.Traversable (mapM, forM)
import           Data.Witherable (Witherable(..))
import qualified Network.AWS.EC2 as EC2 -- (*)
import qualified Network.AWS.ELB as ELB -- (*)
import qualified Network.AWS.Route53 as R53 -- (*)
import           Prelude hiding (filter, mapM, forM)

import           Infracast.Amazonka -- (*)
import           Infracast.NixTypes -- (*)
import           Infracast.Types -- (*)

-- * Elb Matcher

request :: AWS m => Action m Elb ELB.DescribeLoadBalancers
request = \f s -> const s <$> f ELB.describeLoadBalancers

candidates :: AWS m => Elb -> Rs ELB.DescribeLoadBalancers -> m [(Tags, ELB.LoadBalancerDescription)]
candidates Elb{..} es = do
  tagged <- fmap catMaybes . forM (es ^.. ELB.dlbrsLoadBalancerDescriptions . folded) $ \elb -> do
    if not . T.isPrefixOf elb_name $ elb ^. ELB.lbdLoadBalancerName . _Just then return Nothing else Just <$> do
      elb ^. ELB.lbdLoadBalancerName . _Just :| []
           & send . ELB.describeTags
         <&> concat . toListOf (ELB.dtrsTagDescriptions . folded . ELB.tdTags . _Just . to NonEmpty.toList)
         <&> (, elb) . map (\elbTag -> (elbTag ^. ELB.tagKey, elbTag ^. ELB.tagValue . _Just))
  asks ctxTags <&> \want -> filter (\(have, _) -> all (`elem` have) want) tagged


hashesTo :: Elb -> (Tags, ELB.LoadBalancerDescription) -> Bool
hashesTo elb (tags, _) = ("hash", hashOf elb) `elem` tags

-- Given a remote resource, extract its identifier.
extractId :: Traversal' (Tags, ELB.LoadBalancerDescription) ResourceId
-- XXX: If '(_, Nothing)', "" by mempty on Traversal, don't know how bad this is.
extractId = _2 . ELB.lbdLoadBalancerName . _Just


-- * Upcast->Amazonka Type Conversions

toElbTag :: (Text, Text) -> ELB.Tag
toElbTag = \(k, v) -> ELB.tag k & ELB.tagValue ?~ v

toElbListener :: Listener -> ELB.Listener
toElbListener Listener{..} = ELB.listener listener_lbProtocol
                            (fromIntegral listener_lbPort)
                            (fromIntegral listener_instancePort)
                        & ELB.lInstanceProtocol ?~ listener_instanceProtocol
                        & ELB.lSSLCertificateId .~ case listener_sslCertificateId of { "" -> Nothing; x -> Just x }

toElbHealthcheck :: HealthCheck -> ELB.HealthCheck
toElbHealthcheck HealthCheck{..} = ELB.healthCheck
  (toElbHealthcheckTarget healthCheck_target)
  (fromIntegral healthCheck_interval)
  (fromIntegral healthCheck_timeout)
  (fromIntegral healthCheck_unhealthyThreshold)
  (fromIntegral healthCheck_healthyThreshold)
  where
    toElbHealthcheckTarget :: Target -> Text
    toElbHealthcheckTarget = \case
      Http HealthCheckPathTarget{..} ->
        "HTTP:" <> pack (show healthCheckPathTarget_port) <> healthCheckPathTarget_path
      Https HealthCheckPathTarget{..} ->
          "HTTPS:" <> pack (show healthCheckPathTarget_port) <> healthCheckPathTarget_path
      Tcp port ->
          "TCP:" <> pack (show port)
      Ssl port ->
          "SSL:" <> pack (show port)



toElbAccessLog :: AccessLog -> ELB.AccessLog
toElbAccessLog AccessLog{..} = if not accessLog_enable then ELB.accessLog False else ELB.accessLog True
  & ELB.alEmitInterval ?~ fromIntegral accessLog_emitInterval
  & ELB.alS3BucketName ?~ accessLog_s3BucketName
  & ELB.alS3BucketPrefix ?~ accessLog_s3BucketPrefix

toElbConnectionDraining :: ConnectionDraining -> ELB.ConnectionDraining
toElbConnectionDraining ConnectionDraining{..} = ELB.connectionDraining connectionDraining_enable
  & ELB.cdTimeout ?~ fromIntegral connectionDraining_timeout

setElbStickinessPolicy :: AWS m => ResourceId -> Listener -> m ()
setElbStickinessPolicy elb_name Listener{..} = unless (isNothing listener_stickiness) $ do
  let policyName = elb_name <> "-" <> hashOf Listener{..}
  case fromJust listener_stickiness of
    Lb cookieExpiration -> ELB.createLBCookieStickinessPolicy elb_name policyName
      & ELB.clbcspCookieExpirationPeriod .~ fmap fromIntegral cookieExpiration
      & void . send
    App cookieName -> ELB.createAppCookieStickinessPolicy elb_name policyName cookieName
      & void . send
  ELB.setLoadBalancerPoliciesOfListener elb_name (fromIntegral listener_lbPort)
    & ELB.slbpolPolicyNames .~ return policyName
    & void . send


setRoute53Alias :: AWS m => ELB.LoadBalancerDescription -> Route53Alias -> m ()
setRoute53Alias description Route53Alias{..} = do
  let elbZone = description ^. ELB.lbdCanonicalHostedZoneNameId . _Just
      elbDNS  = description ^. ELB.lbdDNSName . _Just
      upsert  = R53.resourceRecordSet route53Alias_name R53.A
              & R53.rrsAliasTarget ?~ R53.aliasTarget elbZone elbDNS False
              & R53.change R53.Upsert
  R53.changeBatch (upsert :| []) & R53.changeResourceRecordSets route53Alias_zoneId  & void . send

describeLoadBalancer :: AWS m => ResourceId -> m ELB.LoadBalancerDescription
describeLoadBalancer elb_name = ELB.describeLoadBalancers
                              & ELB.dlbLoadBalancerNames .~ return elb_name
                              & send <&> head . view ELB.dlbrsLoadBalancerDescriptions

-- * 'Elb' Resource

create :: AWS m => Elb -> m ResourceId
create Elb{..} = do
  let elb_name' = T.take 32 $ elb_name <> "-" <> hashOf Elb{..}
  elb_tags <- asks ctxTags <&> nonEmpty . map toElbTag . (("hash", hashOf Elb{..}):)
  _ <- ELB.createLoadBalancer elb_name'
     & ELB.clbTags .~ elb_tags
     & ELB.clbScheme .~ (if elb_internal then Just "internal" else Nothing)
     & ELB.clbSecurityGroups .~ map (\(RefRemote r) -> r) elb_securityGroups
     & ELB.clbSubnets .~ map (\(RefRemote r) -> r) elb_subnets
     & ELB.clbListeners .~ map toElbListener elb_listeners
     & send
  forM elb_listeners $ setElbStickinessPolicy elb_name'
  toElbHealthcheck elb_healthCheck & ELB.configureHealthCheck elb_name' & send
  ELB.loadBalancerAttributes
    & ELB.lbaAccessLog ?~ toElbAccessLog elb_accessLog
    & ELB.lbaConnectionDraining ?~ toElbConnectionDraining elb_connectionDraining
    & ELB.lbaCrossZoneLoadBalancing ?~ ELB.crossZoneLoadBalancing elb_crossZoneLoadBalancing
    & send . ELB.modifyLoadBalancerAttributes elb_name'
  describeLoadBalancer elb_name' >>= forM elb_route53Aliases . setRoute53Alias
  return elb_name'

update :: AWS m => ResourceId -> Elb -> m ResourceId
update id elb = do
  elb <- create elb
  ELB.deleteLoadBalancer id & defer . send
  return elb

reify :: Map Reference ResourceId -> (Elb -> Either Missing Elb)
reify ledger Elb{..} = do
  elb_securityGroups <- lookup_ ledger (Reference "ec2-sg") `mapM` elb_securityGroups
  elb_subnets <- lookup_ ledger (Reference "ec2-subnet") `mapM` elb_subnets
  return Elb{..}

-- * 'Elbinstanceset' resource

toElbInstance :: InfraRef Ec2instance -> ELB.Instance
toElbInstance ref = ELB.instance' & ELB.iInstanceId .~ case ref of
  RefRemote r -> Just r
  RefLocal  _ -> Nothing

matchInstanceSet :: AWS m => a -> m (Either DiscoveryError MatchResult)
matchInstanceSet = virtual

createInstanceSet Elbinstanceset{..} = "(virtual)" <$ do
  ELB.registerInstancesWithLoadBalancer (fromRefRemote elbinstanceset_elb)
    & ELB.riwlbInstances .~ map toElbInstance elbinstanceset_instances
    & unless (null elbinstanceset_instances) . void . send

updateInstanceSet _ Elbinstanceset{..} = "(virtual)" <$ do
  currentInstances <- ELB.describeLoadBalancers
    & ELB.dlbLoadBalancerNames .~ [fromRefRemote elbinstanceset_elb]
    & send <&> view (ELB.dlbrsLoadBalancerDescriptions . folded . ELB.lbdInstances)
  let intendedInstances = map toElbInstance elbinstanceset_instances
      (attach, detach) = currentInstances `converge` intendedInstances
  ELB.registerInstancesWithLoadBalancer (fromRefRemote elbinstanceset_elb)
    & ELB.riwlbInstances .~ attach
    & unless (null attach) . void . send
  ELB.deregisterInstancesFromLoadBalancer (fromRefRemote elbinstanceset_elb)
    & ELB.diflbInstances .~ detach
    & unless (null detach) . void . send

reifyInstanceSet ledger Elbinstanceset{..} = do
  elbinstanceset_elb <- lookup_ ledger (Reference "elb") elbinstanceset_elb
  elbinstanceset_instances <- lookup_ ledger (Reference "ec2-instance") `mapM` elbinstanceset_instances
  return Elbinstanceset{..}
