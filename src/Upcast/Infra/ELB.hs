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
import qualified Data.HashMap.Strict as H

import Aws.Query (QueryAPIConfiguration(..), castValue)
import qualified Aws.Route53 as R53
import qualified Aws.Ec2 as EC2
import qualified Aws.Elb as ELB

import Upcast.Infra.Types

instance FromJSON ELB.LbProtocol where
    parseJSON (String "http")  = pure ELB.HTTP
    parseJSON (String "https") = pure ELB.HTTPS
    parseJSON (String "tcp")   = pure ELB.TCP
    parseJSON (String "ssl")   = pure ELB.SSL
    parseJSON _ = mzero

data R53Alias = R53Alias Text R53.HostedZoneId

catTuples = foldr (\(ls, sp) (lss, sps) -> (ls:lss, sp:sps)) ([], [])

elbPlan :: (MonadFree InfraF m, Functor m)
        => InstanceA
        -> [(Text, Text)]
        -> [(Text, Text)]
        -> [Value]
        -> m ()
elbPlan instanceA sgA subnetA elbs = do
    elb1 <- fmap mconcat $ forM elbs $ \elb -> do
        let (clb, attrs, machines, r53, lbStickiness, healthCheck) = parse elb $ \(Object obj) -> do
              clb_name :: Text <- obj .: "name"
              internal :: Bool <- obj .: "internal"
              let clb_scheme = if internal then ELB.Internal else ELB.Public
              securityGroups :: [Text] <- obj .: "securityGroups"
              let clb_securityGroupIds = catMaybes $ lookupOrId "sg-" sgA <$> securityGroups
              subnets :: [Text] <- obj .: "subnets"
              let clb_subnetIds = catMaybes $ lookupOrId "subnet-" subnetA <$> subnets

              listeners :: [Value] <- obj .: "listeners"

              (clb_listeners, lbStickiness) <- fmap catTuples $ forM listeners $ \(Object listener) -> do
                    l_lbPort <- listener .: "lbPort"
                    l_instancePort <- listener .: "instancePort"
                    l_instanceProtocol <- listener .: "instanceProtocol"
                    l_lbProtocol <- listener .: "lbProtocol"
                    sslCertificateId <- listener .:? "sslCertificateId"

                    let l_sslCertificateId = case sslCertificateId of
                                                Just "" -> Nothing
                                                Nothing -> Nothing
                                                Just x -> Just x

                    lbStickinessExp :: Maybe Integer <- listener .:? "lbStickinessCookieExpiration"

                    return (ELB.Listener{..}, (maybe (-1) id lbStickinessExp, l_lbPort))

              let clb = ELB.CreateLoadBalancer{..}

              crossAttr <-
                    ELB.CrossZoneLoadBalancing <$> (obj .: "crossZoneLoadBalancing")

              Object alog <- obj .: "accessLog"
              accessAttr <-
                    ELB.AccessLog <$> (alog .: "enable")
                                  <*> fmap (\case (5 :: Int) -> ELB.Min5
                                                  (60 :: Int) -> ELB.Min60)
                                          (alog .: "emitInterval")
                                  <*> (alog .: "s3BucketName")
                                  <*> (alog .: "s3BucketPrefix")

              Object drain <- obj .: "connectionDraining"
              drainingAttr <-
                    ELB.ConnectionDraining <$> (drain .: "enable")
                                           <*> (drain .: "timeout")

              Object hc <- obj .: "healthCheck"
              Object hcTarget <- hc .: "target"
              target <- do
                proto :: Text <- hcTarget .: "protocol"
                return $ case proto of
                           "TCP" -> ELB.TargetTCP <$> (hcTarget .: "port")
                           "SSL" -> ELB.TargetSSL <$> (hcTarget .: "port")
                           "HTTP" -> ELB.TargetHTTP <$> (hcTarget .: "port") <*> (hcTarget .: "path")
                           "HTTPS" -> ELB.TargetHTTPS <$> (hcTarget .: "port") <*> (hcTarget .: "path")

              healthCheck <-
                    ELB.HealthCheck <$> target
                                    <*> (hc .: "healthyThreshold")
                                    <*> (hc .: "unhealthyThreshold")
                                    <*> (hc .: "interval")
                                    <*> (hc .: "timeout")

              machines :: [Text] <- obj .: "machines"

              Object aliases <- obj .: "route53Aliases"
              r53 <- H.elems <$> H.traverseWithKey (\k (Object v) -> toAliasCRR k <$> v .: "zoneId") aliases

              return (clb, [crossAttr, accessAttr, drainingAttr], machines, r53, lbStickiness, healthCheck)

        aws_ clb

        let name = ELB.clb_name clb
        wait $ ELB.DescribeLoadBalancers [name]

        aws_ $ ELB.ModifyLoadBalancerAttributes name attrs

        forM lbStickiness $ \(cookieExp, lbPort) -> when (cookieExp /= -1) $ do
          let policyName = mconcat [name, "-", T.pack $ show lbPort, "-cookie-exp-", T.pack $ show cookieExp]
          aws_ $ ELB.CreateLBCookieStickinessPolicy name (if cookieExp == 0 then Nothing else Just cookieExp) policyName
          aws_ $ ELB.SetLoadBalancerPoliciesOfListener name (fromIntegral lbPort) [policyName]

        aws_ $ ELB.ConfigureHealthCheck name healthCheck

        let instances = fmap fst $ catMaybes $ fmap (flip lookup instanceA) machines
        aws_ $ ELB.RegisterInstancesWithLoadBalancer name instances

        Array elbInfos <- aws (ELB.DescribeLoadBalancers [name])
        let [elbInfo] = V.toList elbInfos

        let (elbZoneId :: Text, elbName :: Text) = parse elbInfo $ \(Object obj) -> do
              (,) <$> obj .: "CanonicalHostedZoneNameID" <*> obj .: "DNSName"

        let crr = (\x -> x elbName elbZoneId) <$> r53
        return [(clb, attrs, crr)]

    fmap mconcat $ forM elb1 $ \(clb, attrs, crr) -> do
      mapM_ aws53crr crr
      return [(clb, attrs)]

    return ()

toAliasCRR domain zoneId = \elbName elbZoneId ->
    R53.ChangeResourceRecordSets
      (R53.HostedZoneId zoneId)
      Nothing
      [(R53.UPSERT, R53.ResourceRecordSet { R53.rrsName = R53.Domain domain
                                          , R53.rrsType = R53.A
                                          , R53.rrsAliasTarget = Just R53.AliasTarget { R53.atHostedZoneId = R53.HostedZoneId elbZoneId
                                                                                      , R53.atDNSName = R53.Domain elbName
                                                                                      }
                                          , R53.rrsSetIdentifier = Nothing
                                          , R53.rrsWeight = Nothing
                                          , R53.rrsRegion = Nothing
                                          , R53.rrsTTL = Nothing
                                          , R53.rrsRecords = []
                                          })]
