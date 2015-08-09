{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Upcast.Infra.Amazonka where

import           Prelude (error)
import           Network.AWS.Prelude

import           Data.List.NonEmpty (head, nonEmpty)

import           Data.Char (isAsciiLower)
import           Data.Foldable (for_)

import           Control.Applicative
import           Control.Lens hiding ((.=))
import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.Trans.AWS (MonadAWS, AWSRequest, AWSPager, send, sendCatch, info)
import qualified Control.Monad.Trans.AWS as AWS
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Builder as Build
import           Data.ByteString.Lazy (toStrict)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, maybeToList)
import           Data.Monoid
import           Data.Proxy
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Data.Traversable (traverse)
import qualified Network.AWS.Data as AWS
import qualified Network.AWS.EC2 as EC2
import qualified Network.AWS.EC2.Types as EC2
import qualified Network.AWS.ELB as ELB
import qualified Network.AWS.ELB.Types as ELB
import qualified Network.AWS.Route53 as R53
import qualified Network.AWS.Route53.Types as R53
import           Upcast.IO (expectRight)
import           Upcast.Infra.Match
import           Upcast.Infra.NixTypes
import           Upcast.Infra.Types
import           Upcast.Shell (exec, fgconsume)

import           Upcast.Infra.Amazonka.UpsertAlias
import           Upcast.Types (Machine(..))


true = Just (EC2.attributeBooleanValue & EC2.abvValue .~ Just True)

createTags xs tags =
  void . send $ EC2.createTags & EC2.ct1Resources .~ xs
                               & EC2.ct1Tags .~ map (uncurry EC2.tag) tags

createVpc :: MonadAWS m => Tags -> Text -> Ec2vpc -> m ResourceId
createVpc defTags aname Ec2vpc{..} = do
  Just vpc <- view EC2.cvrVpc <$> send (EC2.createVpc ec2vpc_cidrBlock)
  let vpcId = vpc ^. EC2.vpcVpcId

  void . send $ EC2.describeVpcs & EC2.dv1VpcIds .~ [vpcId]
  createTags [vpcId] (("Name", aname):defTags)

  internetAccess defTags vpcId
  return vpcId

internetAccess :: MonadAWS m => Tags -> ResourceId -> m ()
internetAccess defTags vpcId = do
  void . send $ EC2.modifyVpcAttribute vpcId
    & EC2.mvaEnableDnsHostnames .~ true
    & EC2.mvaEnableDnsSupport .~ true

  Just igw <- view EC2.cigrInternetGateway <$> send EC2.createInternetGateway
  let gwId = igw ^. EC2.igInternetGatewayId

  void . send $ EC2.attachInternetGateway gwId vpcId

  createTags [gwId] defTags

  rtb:_rtbs <- view EC2.drtrRouteTables <$>
          send (EC2.describeRouteTables & EC2.drt2Filters .~ filterIds "vpc-id" [vpcId])

  let Just rtbId = rtb ^. EC2.rtRouteTableId

  void . send $ EC2.createRoute rtbId "0.0.0.0/0" & EC2.crGatewayId .~ Just gwId

  createTags [rtbId] defTags

createSubnet :: MonadAWS m =>  ResourceId -> Tags -> Text -> Ec2subnet -> m ResourceId
createSubnet vpcId defTags aname Ec2subnet{..} = do
  Just subnet <- view EC2.csrSubnet <$>
                 send (EC2.createSubnet vpcId ec2subnet_cidrBlock
                       & EC2.cs1AvailabilityZone .~ Just ec2subnet_zone)

  let subnetId = subnet ^. EC2.s1SubnetId
  void . send $ matchIds [subnetId] (Proxy :: Proxy Ec2subnet)
  createTags [subnetId] defTags
  return subnetId

ruleToPermission :: Rule -> EC2.IpPermission
ruleToPermission Rule{..} = EC2.ipPermission rule_protocol
                            & EC2.ipFromPort .~ (fromIntegral <$> rule_fromPort)
                            & EC2.ipToPort .~ (fromIntegral <$> rule_toPort)
                            & EC2.ipIpRanges .~ (EC2.ipRange <$> maybeToList rule_sourceIp)

createSecurityGroup :: MonadAWS m => Maybe ResourceId -> Tags -> Text -> Ec2sg -> m ResourceId
createSecurityGroup vpcId defTags aname Ec2sg{..} = do
  groupId <- view EC2.csgrGroupId <$>
             send (EC2.createSecurityGroup ec2sg_name ec2sg_description
                   & EC2.csgVpcId .~ vpcId)

  void . send $ matchIds [groupId] (Proxy :: Proxy Ec2sg)

  void . send $ EC2.authorizeSecurityGroupIngress
    & EC2.asgiGroupId .~ Just groupId
    & EC2.asgiIpPermissions .~ map ruleToPermission ec2sg_rules

  createTags [groupId] defTags

  return groupId

toVolumeType Gp2 = EC2.Gp2
toVolumeType (Iop _) = EC2.Io1
toVolumeType Standard = EC2.Standard

createVolume :: MonadAWS m => Tags -> Text -> Ebs -> m ResourceId
createVolume defTags aname Ebs{..} = do
  volumeId <- view EC2.cvrVolumeId <$>
              send (EC2.createVolume ebs_zone
                    & EC2.cv1SnapshotId .~ ebs_snapshot
                    & EC2.cv1VolumeType .~ Just (toVolumeType ebs_volumeType)
                    & EC2.cv1Iops .~ (case ebs_volumeType of
                                       Iop n -> Just (fromIntegral n)
                                       _ -> Nothing)
                    & EC2.cv1Size .~ Just (fromIntegral ebs_size))

  void . send $ matchIds [volumeId] (Proxy :: Proxy Ebs)
  createTags [volumeId] (("Name", ebs_name):defTags)
  return volumeId


-- we can only handle emphemeral volumes at creation time
xformMapping :: BlockDeviceMapping -> Maybe EC2.BlockDeviceMapping
xformMapping BlockDeviceMapping{..} =
  case blockDeviceMapping_disk of
   RefLocal _ -> fail "ignore (handled later)"
   RefRemote x | "ephemeral" `T.isPrefixOf` x ->
                 return (EC2.blockDeviceMapping blockDeviceMapping_blockDeviceMappingName
                         & EC2.bdmVirtualName .~ Just x)
               | otherwise ->
                 fail "ignore (not implemented)"


createInstance :: MonadAWS m
                  => Tagged Ec2subnet (Maybe ResourceId)
                  -> Tagged Ec2sg [ResourceId]
                  -> Maybe (Attrs Text) -- | UserData
                  -> Tagged Ec2keypair (Maybe ResourceId)
                  -> Tags
                  -> Text
                  -> Ec2instance
                  -> m ResourceId
createInstance (unTagged -> subnetId)
               (unTagged -> securityGroupIds)
               udata
               (unTagged -> keyName)
               defTags
               name
               Ec2instance{..} = do
  let bds = Map.toList (xformMapping <$> ec2instance_blockDeviceMapping)
  let type' = case AWS.fromText ec2instance_instanceType of
                Left e -> error $ "unknown instance type: " ++ T.unpack ec2instance_instanceType  ++ " " ++ e
                Right t -> t


  inst:_ <- view EC2.rirInstances <$> send (
    EC2.runInstances ec2instance_ami 1 1
    & EC2.riInstanceType .~ Just type'
    & EC2.riUserData .~ userData name udata
    & EC2.riKeyName .~ keyName
    & EC2.riEbsOptimized .~ Just ec2instance_ebsOptimized
    & EC2.riPlacement .~ Just (EC2.placement
                               & EC2.pAvailabilityZone .~ Just ec2instance_zone)
    & EC2.riIamInstanceProfile .~ Just (EC2.iamInstanceProfileSpecification
                                        & EC2.iipsArn .~ ec2instance_instanceProfileARN)
    & EC2.riNetworkInterfaces .~ [EC2.instanceNetworkInterfaceSpecification
                                  & EC2.inisAssociatePublicIpAddress .~ Just True
                                  & EC2.inisSubnetId .~ subnetId
                                  & EC2.inisGroups .~ securityGroupIds
                                  & EC2.inisDeviceIndex .~ Just 0]
    & EC2.riMonitoring .~ Just (EC2.runInstancesMonitoringEnabled True)
    & EC2.riBlockDeviceMappings .~ catMaybes (snd <$> bds))

  let instanceId = inst ^. EC2.i1InstanceId

  createTags [instanceId] (("Name", name):defTags)

  return instanceId
  where
    base64Object = T.decodeUtf8 . Base64.encode . toStrict . encode . object

    userData _ Nothing = Nothing
    userData name udata = Just (base64Object (mconcat
                                              [ [ "hostname" .= name ]
                                              , maybe [] (Map.toList . fmap String) udata
                                              ]))

say :: (MonadAWS m, Show a) => Build.Builder -> a -> m ()
say msg = info . mappend msg . Build.stringUtf8 . show

attachVolume :: MonadAWS m
                => Tagged Ec2instance ResourceId
                -> Tagged Ebs ResourceId
                -> Text
                -> m ()
attachVolume (unTagged -> instanceId) (unTagged -> volumeId) device = do
  result <- sendCatch $ EC2.attachVolume volumeId instanceId device
  case result of
   Right _ -> return ()
   Left e@(AWS.ServiceError _ _ sve :: AWS.ServiceError EC2.EC2Error) ->
     say "Did not attach volume: "
         (instanceId, (sve ^. EC2.errErrors & head) ^. EC2.msgMessage)

importKeypair :: MonadAWS m => Base64 -> Tags -> Text -> Ec2keypair -> m ResourceId
importKeypair base64 _ _ Ec2keypair{..} = do
  response <- send $ EC2.importKeyPair ec2keypair_name base64
  let Just name = response ^. EC2.ikprKeyName
  return name

elbTags = fromJust . nonEmpty . map (\(k, v) -> ELB.tag k & ELB.tagValue .~ Just v)

createElb :: MonadAWS m
             => Tagged Ec2sg [ResourceId]
             -> Tagged Ec2subnet [ResourceId]
             -> Tagged Ec2instance [ResourceId]
             -> Tags
             -> Text
             -> Elb
             -> m ResourceId
createElb (unTagged -> secGroupIds)
          (unTagged -> subnetIds)
          (unTagged -> instanceIds)
          (elbTags -> tags)
          aname
          Elb{..} = do
  send (ELB.createLoadBalancer elb_name tags
        & ELB.clbScheme .~ (if elb_internal then Just "internal" else Nothing)
        & ELB.clbSecurityGroups .~ secGroupIds
        & ELB.clbSubnets .~ subnetIds
        & ELB.clbListeners .~ map fromListener elb_listeners)

  describe <- send $ ELB.describeLoadBalancers & ELB.dlbLoadBalancerNames .~ [elb_name]

  for_ elb_listeners (applyStickiness elb_name)

  void . send $ ELB.configureHealthCheck elb_name (healthcheck elb_healthCheck)

  unless (null instanceIds) $
    void $ send (ELB.registerInstancesWithLoadBalancer elb_name
                 & ELB.riwlbInstances .~ map instance' instanceIds)

  let attrs = ELB.loadBalancerAttributes
              & ELB.lbaAccessLog .~ Just (accessLog elb_accessLog)
              & ELB.lbaConnectionDraining .~ Just (connectionDraining elb_connectionDraining)
              & ELB.lbaCrossZoneLoadBalancing .~ Just (ELB.crossZoneLoadBalancing elb_crossZoneLoadBalancing)
  send $ ELB.modifyLoadBalancerAttributes elb_name attrs

  let [description] = describe ^. ELB.dlbrLoadBalancerDescriptions
  _ <- forAttrs elb_route53Aliases (aliasTargets description)

  return elb_name

  where
    fromListener Listener{..} =
      ELB.listener listener_lbProtocol (fromIntegral listener_lbPort) (fromIntegral listener_instancePort)
      & ELB.lInstanceProtocol .~ Just listener_instanceProtocol
      & ELB.lSSLCertificateId .~ if listener_sslCertificateId == ""
                                 then Nothing
                                 else Just listener_sslCertificateId

    instance' id = ELB.instance' & ELB.iInstanceId .~ Just id

    healthcheck HealthCheck{..} =
      ELB.healthCheck target
        (fromIntegral healthCheck_interval)
        (fromIntegral healthCheck_timeout)
        (fromIntegral healthCheck_unhealthyThreshold)
        (fromIntegral healthCheck_healthyThreshold)
      where
        target = case healthCheck_target of
          Http HealthCheckPathTarget{..} ->
            "HTTP:" <> T.pack (show healthCheckPathTarget_port) <> healthCheckPathTarget_path
          Https HealthCheckPathTarget{..} ->
            "HTTPS:" <> T.pack (show healthCheckPathTarget_port) <> healthCheckPathTarget_path
          Tcp port ->
            "TCP:" <> T.pack (show port)
          Ssl port ->
            "SSL:" <> T.pack (show port)

    accessLog AccessLog{..} =
      if not accessLog_enable
      then ELB.accessLog False
      else ELB.accessLog True
           & ELB.alEmitInterval .~ Just (fromIntegral accessLog_emitInterval) -- TODO: 5 or 60
           & ELB.alS3BucketName .~ Just accessLog_s3BucketName
           & ELB.alS3BucketPrefix .~ Just accessLog_s3BucketPrefix

    connectionDraining ConnectionDraining{..} =
      ELB.connectionDraining connectionDraining_enable
      & ELB.cdTimeout .~ Just (fromIntegral connectionDraining_timeout)

    stickinessPolicyName :: Stickiness -> Text
    stickinessPolicyName (App x) = "app-" <> T.filter isAsciiLower x
    stickinessPolicyName (Lb Nothing) = "lb-no-exp"
    stickinessPolicyName (Lb (Just x)) ="lb-exp-" <> T.pack (show x)

    applyStickiness elb_name Listener{..} =
      case listener_stickiness of
       Nothing -> return ()
       Just policy -> do
         let policyName = elb_name <> "-" <> T.pack (show listener_lbPort)
                          <> "-cookie-" <> stickinessPolicyName policy

         case policy of
          Lb cookieExp ->
            void $ send (ELB.createLBCookieStickinessPolicy elb_name policyName
                         & ELB.clbcspCookieExpirationPeriod .~ fmap fromIntegral cookieExp)
          App cookieName ->
            void $ send (ELB.createAppCookieStickinessPolicy elb_name policyName cookieName)

         send (ELB.setLoadBalancerPoliciesOfListener elb_name (fromIntegral listener_lbPort)
               & ELB.slbpolPolicyNames .~ [policyName])
         return ()

    aliasTargets description aname Route53Alias{..} = do
      let Just elbZone = description ^. ELB.lbdCanonicalHostedZoneNameID
      let Just elbDNS = description ^. ELB.lbdDNSName
      let upsert = UpsertAlias { uaHostedZoneId = route53Alias_zoneId
                               , uaName = aname
                               , uaRecordType = R53.A
                               , uaTarget= R53.aliasTarget elbZone elbDNS False
                               }
      void $ send upsert

attachableVolume volumeA BlockDeviceMapping{..} =
  case blockDeviceMapping_disk of
   RefLocal _ -> lookupOrId volumeA blockDeviceMapping_disk
   RefRemote x | "ephemeral" `T.isPrefixOf` x -> fail "ignore (handled already)"
               | "vol-" `T.isPrefixOf` x -> return x
               | otherwise -> error $ "can't handle disk: " ++ T.unpack x

ensure create tags k v = do
  discovery <- discover v (("Name", k):tags)
  case discovery of
   Right one -> return one
   Left NotFound -> create tags k v
   Left (Ambiguous many) ->
     error $ "required intervention, ambiguous results: " ++ show (k, many)

plan :: MonadAWS m
        => Text
        -> Attrs (Attrs Text)
        -> Attrs (Ec2keypair, Base64)
        -> Infras
        -> m [Machine]
plan expressionName userData keypairs Infras{..} =
  let
    defTags = [ ("created-using", "upcast")
              , ("realm", infraRealmName)
              , ("expression", expressionName)
              ]
  in do
    vpcA <- forAttrs infraEc2vpc (ensure createVpc defTags)

    subnetA <-
      forAttrs infraEc2subnet (\k v -> ensure (createSubnet (subnetLookupVpc vpcA v)) defTags k v)
    sgA <-
      forAttrs infraEc2sg (\k v -> ensure (createSecurityGroup (sgLookupVpc vpcA v)) defTags k v)
    volumeA <-
      forAttrs infraEbs (ensure createVolume defTags)
    keypairA <-
      forAttrs keypairs (\k (v, base64) -> ensure (importKeypair base64) defTags k v)
    instanceA <-
      forAttrs infraEc2instance (\k v -> ensure (createInstance
                                                 (instanceLookupSubnet subnetA v)
                                                 (instanceLookupSg sgA v)
                                                 (instanceLookupUserData userData k)
                                                 (instanceLookupKeypair keypairA v))
                                         defTags
                                         k
                                         v)

    void $ send (EC2.describeInstanceStatus & EC2.disInstanceIds .~ map snd instanceA)

    forAttrs infraEc2instance $ \aname Ec2instance{..} ->
      forAttrs ec2instance_blockDeviceMapping $ \vname bdev ->
        case attachableVolume volumeA bdev of
         Just id ->
           attachVolume (tagWith Proxy (lookup_ instanceA (RefLocal aname))) (tagWith Proxy id) vname
         Nothing -> return ()

    elbA <-
      forAttrs infraElb (\k v -> ensure (createElb
                                         (elbLookupSgs sgA v)
                                         (elbLookupSubnets subnetA v)
                                         (elbLookupInstances instanceA v))
                                 defTags
                                 k
                                 v)

    machines instanceA keypairs infraEc2instance

machines [] _ _ = return []
machines instanceA keypairs infraEc2instance = fmap toMachine (send request)
  where
    request = EC2.describeInstances & EC2.di1InstanceIds .~ map fst nameById
    nameById = map (\(k, v) -> (v, k)) instanceA
    keypairFileByName =
      map (\(Ec2keypair{..}, _) -> (ec2keypair_name, ec2keypair_privateKeyFile)) $ Map.elems keypairs

    toMachine resp = do
      rs <- resp ^. EC2.dirReservations
      inst <- rs ^. EC2.rInstances
      let id = inst ^. EC2.i1InstanceId
      let Just name = lookup id nameById
      let Just publicIp = inst ^. EC2.i1PublicIpAddress
      let Just privateIp = inst ^. EC2.i1PrivateIpAddress
      let keyFile = inst ^. EC2.i1KeyName >>= (`lookup` keypairFileByName)
      return $ Machine name publicIp privateIp id keyFile


-- These have to be removed first thing:

subnetLookupVpc vpcA Ec2subnet{..} = lookup_ vpcA ec2subnet_vpc
sgLookupVpc vpcA Ec2sg{..} = ec2sg_vpc >>= lookupOrId vpcA
instanceLookupSubnet subnetA Ec2instance{..} = tagWith Proxy $ ec2instance_subnet >>= lookupOrId subnetA
instanceLookupSg sgA Ec2instance{..} = tagWith Proxy $ lookup_ sgA <$> ec2instance_securityGroups
instanceLookupKeypair keypairA Ec2instance{..} = tagWith Proxy $ lookupOrId keypairA ec2instance_keyPair
instanceLookupUserData = flip Map.lookup
elbLookupSubnets subnetA Elb{..} = tagWith Proxy $ lookup_ subnetA <$> elb_subnets
elbLookupSgs sgA Elb{..} = tagWith Proxy $ lookup_ sgA <$> elb_securityGroups
elbLookupInstances instanceA Elb{..} = tagWith Proxy $ lookup_ instanceA <$> elb_instances
