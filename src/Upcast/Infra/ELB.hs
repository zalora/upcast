{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , ScopedTypeVariables
           , DeriveFunctor
           , FlexibleContexts
           , LambdaCase
           #-}

module Upcast.Infra.ELB where

import Control.Applicative
import Control.Monad
import Control.Monad.Free

import Data.Monoid
import Data.Char
import Data.Maybe (catMaybes)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson.Types as A
import qualified Data.Vector as V

import Aws.Query (QueryAPIConfiguration(..), castValue)
import qualified Aws.Route53 as R53
import qualified Aws.Ec2 as EC2
import qualified Aws.Elb as ELB
import qualified Aws.Elb.Commands.CreateAppCookieStickinessPolicy as ELB -- Should be part of ELB in the future

import Upcast.Infra.Types
import Upcast.Infra.Nix


stickinessPolicyName :: Stickiness -> Text
stickinessPolicyName (App x) = T.append "app-" (T.filter isAsciiLower x)
stickinessPolicyName (Lb Nothing) = "lb-no-exp"
stickinessPolicyName (Lb (Just x)) = T.append "lb-exp-" . T.pack $ show x

elbPlan :: (MonadFree InfraF m, Applicative m)
        => IDAlist
        -> IDAlist
        -> IDAlist
        -> Attrs Elb
        -> m [(Text, ())]
elbPlan instanceA sgA subnetA elbs =
  forAttrs elbs $ \clb_name Elb{..} -> do
    -- create
    let clb_scheme = if elb_internal then ELB.Internal else ELB.Public
    let clb_securityGroupIds = catMaybes $ lookupOrId' "sg-" sgA <$> elb_securityGroups
    let clb_subnetIds = catMaybes $ lookupOrId' "subnet-" subnetA <$> elb_subnets
    let clb_listeners = map xformListener elb_listeners
    aws_ ELB.CreateLoadBalancer{..}
    wait $ ELB.DescribeLoadBalancers [clb_name]

    -- stickiness
    mapM_ (applyStickiness clb_name) elb_listeners

    -- attributes
    let crossAttr = ELB.CrossZoneLoadBalancing elb_crossZoneLoadBalancing
    let accessAttr = xformAccessLog elb_accessLog
    let drainingAttr = xformConnectionDraining elb_connectionDraining
    aws_ $ ELB.ModifyLoadBalancerAttributes clb_name [crossAttr, accessAttr, drainingAttr]

    -- healthcheck
    aws_ $ ELB.ConfigureHealthCheck clb_name (xformHealthCheck elb_healthCheck)

    -- instances
    let instances = catMaybes $ lookupOrId' "i-" instanceA <$> elb_instances
    aws_ $ ELB.RegisterInstancesWithLoadBalancer clb_name instances

     -- DNS aliases
    let r53 = Map.elems $ Map.mapWithKey (\k Route53Aliases{..} -> toAliasCRR k route53Aliases_zoneId) elb_route53Aliases

    Array elbInfos <- aws (ELB.DescribeLoadBalancers [clb_name])
    let [elbInfo] = V.toList elbInfos -- fails in debug mode

    let (elbZoneId :: Text, dnsName :: Text) =
          parse elbInfo $ \(Object obj) ->
            (,) <$> obj .: "CanonicalHostedZoneNameID" <*> obj .: "DNSName"

    let crr = (\x -> x dnsName elbZoneId) <$> r53
    mapM_ aws53crr crr

    return ()

applyStickiness clb_name Listeners{..} =
  case listeners_stickiness of
    Nothing -> return ()
    Just policy -> do
      let policyName = mconcat [ clb_name
                               , "-"
                               , T.pack $ show listeners_lbPort
                               , "-cookie-"
                               , stickinessPolicyName policy
                               ]
      case policy of
        App cookieName -> aws_ $ ELB.CreateAppCookieStickinessPolicy clb_name cookieName policyName
        Lb cookieExp -> aws_ $ ELB.CreateLBCookieStickinessPolicy clb_name cookieExp policyName
      aws_ $ ELB.SetLoadBalancerPoliciesOfListener clb_name (fromIntegral listeners_lbPort) [policyName]

xformConnectionDraining ConnectionDraining{..} =
  ELB.ConnectionDraining connectionDraining_enable (fromIntegral connectionDraining_timeout)

xformAccessLog AccessLog{..} =
  ELB.AccessLog accessLog_enable interval accessLog_s3BucketName accessLog_s3BucketPrefix
  where
    interval =
      case accessLog_emitInterval of
        5 -> ELB.Min5
        60 -> ELB.Min60

xformListener Listeners{..} = ELB.Listener{..}
  where
    l_lbPort = fromIntegral listeners_lbPort
    l_instancePort = fromIntegral listeners_instancePort
    l_lbProtocol = proto listeners_lbProtocol
    l_instanceProtocol = proto listeners_instanceProtocol
    l_sslCertificateId = if listeners_sslCertificateId == ""
                         then Nothing
                         else Just listeners_sslCertificateId

    proto :: Text -> ELB.LbProtocol
    proto "http" = ELB.HTTP
    proto "https" = ELB.HTTPS
    proto "tcp" = ELB.TCP
    proto "ssl" = ELB.SSL
    proto x = error $ mconcat ["xformListener: bad protocol: ", show x]

xformHealthCheck HealthCheck{..} = ELB.HealthCheck{..}
  where
    hc_healthyThreshold = healthCheck_healthyThreshold
    hc_unhealthyThreshold = healthCheck_unhealthyThreshold
    hc_interval = healthCheck_interval
    hc_timeout = healthCheck_timeout
    hc_target =
      case healthCheck_target of
        Http HealthCheckPathTarget{..} ->
          ELB.TargetHTTP healthCheckPathTarget_port healthCheckPathTarget_path
        Https HealthCheckPathTarget{..} ->
          ELB.TargetHTTPS healthCheckPathTarget_port healthCheckPathTarget_path
        Tcp port ->
          ELB.TargetTCP port
        Ssl port ->
          ELB.TargetSSL port

toAliasCRR :: Text -> Text -> (Text -> Text -> R53.ChangeResourceRecordSets)
toAliasCRR domain zoneId = \dnsName elbZoneId ->
    R53.ChangeResourceRecordSets
      (R53.HostedZoneId zoneId)
      Nothing
      [(R53.UPSERT, R53.ResourceRecordSet { R53.rrsName = R53.Domain domain
                                          , R53.rrsType = R53.A
                                          , R53.rrsAliasTarget = Just R53.AliasTarget { R53.atHostedZoneId = R53.HostedZoneId elbZoneId
                                                                                      , R53.atDNSName = R53.Domain dnsName
                                                                                      }
                                          , R53.rrsSetIdentifier = Nothing
                                          , R53.rrsWeight = Nothing
                                          , R53.rrsRegion = Nothing
                                          , R53.rrsTTL = Nothing
                                          , R53.rrsRecords = []
                                          })]
