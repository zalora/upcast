{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , ScopedTypeVariables
           , DeriveFunctor
           , FlexibleContexts
           , LambdaCase
           #-}

module Upcast.Resource.ELB where

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

import Upcast.Resource.Types

instance FromJSON ELB.LbProtocol where
    parseJSON (String "http")  = pure ELB.HTTP
    parseJSON (String "https") = pure ELB.HTTPS
    parseJSON (String "tcp")   = pure ELB.TCP
    parseJSON (String "ssl")   = pure ELB.SSL
    parseJSON _ = mzero

instance FromJSON ELB.Listener where
    parseJSON (Object obj) = do
      l_lbPort <- obj .: "lbPort"
      l_instancePort <- obj .: "instancePort"
      l_instanceProtocol <- obj .: "instanceProtocol"
      l_lbProtocol <- obj .: "lbProtocol"
      sslCertificateId <- obj .:? "sslCertificateId"
      let l_sslCertificateId = case sslCertificateId of
                                  Just "" -> Nothing
                                  Nothing -> Nothing
                                  Just x -> Just x
      return ELB.Listener{..}

data R53Alias = R53Alias Text R53.HostedZoneId

elbPlan instanceA sgA subnetA elbs = do
    fmap mconcat $ forM elbs $ \elb -> do
        let (clb, attrs, machines, r53) = parse elb $ \(Object obj) -> do
              clb_name :: Text <- obj .: "name"
              internal :: Bool <- obj .: "internal"
              let clb_scheme = if internal then ELB.Internal else ELB.Public
              clb_listeners :: [ELB.Listener] <- obj .: "listeners"
              securityGroups :: [Text] <- obj .: "securityGroups"
              let clb_securityGroupIds = catMaybes $ fmap (flip lookup sgA) securityGroups
              subnets :: [Text] <- obj .: "subnets"
              let clb_subnetIds = catMaybes $ fmap (flip lookup subnetA) subnets

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

              machines :: [Text] <- obj .: "machines"

              Object aliases <- obj .: "route53Aliases"
              r53 <- H.elems <$> H.traverseWithKey (\k (Object v) -> toAliasCRR k <$> v .: "zoneId") aliases

              return (clb, [crossAttr, accessAttr, drainingAttr], machines, r53)

        aws_ clb

        let name = ELB.clb_name clb
        wait $ ELB.DescribeLoadBalancers [name]

        aws_ $ ELB.ModifyLoadBalancerAttributes name attrs

        let instances = fmap fst $ catMaybes $ fmap (flip lookup instanceA) machines
        aws_ $ ELB.RegisterInstancesWithLoadBalancer name instances


        Array elbInfos <- aws (ELB.DescribeLoadBalancers [name])
        let [elbInfo] = V.toList elbInfos

        let (elbZoneId :: Text, elbName :: Text) = parse elbInfo $ \(Object obj) -> do
              (,) <$> obj .: "CanonicalHostedZoneNameID" <*> obj .: "CanonicalHostedZoneName"

        mapM_ aws53crr $ (\x -> x elbName elbZoneId) <$> r53


        return [(clb, attrs)]

toAliasCRR domain zoneId = \elbName elbZoneId ->
    R53.ChangeResourceRecordSets
      (R53.HostedZoneId zoneId)
      Nothing
      [(R53.CREATE, R53.ResourceRecordSet { R53.rrsName = R53.Domain domain
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
