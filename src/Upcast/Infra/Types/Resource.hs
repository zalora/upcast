{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ViewPatterns           #-}

module Upcast.Infra.Types.Resource
( Resource(..)
, MatchResult(..)
, Missing(..)
, DiscoveryError(..)
) where

import Prelude hiding (readFile, filter, mapM, forM)
import Control.Applicative (Applicative(..), (<$>), (<*>), (<$), empty, pure, (<|>))
import Control.Lens hiding (Context) -- (*)
import Control.Lens.Action
import Control.Monad (void, (<=<), unless, filterM)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (modify)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.AWS (Rs, AWSPager(..), send, await, paginate, AWST, runAWST)
import Control.Monad.Trans.Resource (ResourceT, runResourceT, register, liftResourceT, ReleaseKey)
import Data.Aeson (encode)
import Data.Aeson.Lens () -- Plated Value
import Data.Aeson.Types (Value(String), object)
import qualified Data.ByteString.Base64 as Base64 (encode)
import Data.ByteString.Lazy (toStrict)
import Data.Conduit (runConduit, tryC, fuse)
import Data.Conduit.List (consume)
import Data.IP ((>:>), AddrRange(..), IPv4)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Data.Map (Map, toList, insert, elems)
import qualified Data.Map as Map (lookup)
import Data.Maybe (fromMaybe, maybeToList, fromJust, isNothing)
import Data.Monoid ((<>))
import Data.Hashable (Hashable(..))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T (null, isPrefixOf, isSuffixOf, intercalate, take)
import qualified Data.Text.IO as T (readFile)
import Data.Traversable (mapM, forM)
import Data.Witherable (Witherable(..))
import qualified Network.AWS.Data as AWS (fromText)
import qualified Network.AWS.EC2 as EC2 -- (*)
import qualified Network.AWS.ELB as ELB -- (*)
import qualified Network.AWS.Route53 as R53 -- (*)
import Network.AWS.Types (Error)
import Upcast.IO (expectRight)
import Upcast.Infra.Types.Amazonka (ResourceId)
import Upcast.Infra.NixTypes -- (*)
import Upcast.Infra.Types.Common (AWS, Context(..), Reference(..), State(..), Tags)
import Upcast.Shell (exec, fgconsume)

class Matcher a b | a -> b, b -> a where
  -- The API action called to match the infra 'a'. Usually an instance of
  -- Amazonka's 'AWSRequest'; such that 'Rs (Rq a)' is defined.
  type Rq a

  -- Construct a request for remote resources which may match the local
  -- resource. Note that post-request processing is possible with
  -- 'candidates'.
  request :: AWS m => Action m a (Rq a)

  -- Any required post-'request' filters, operating on remote resources. It's
  -- best to delegate filters to 'request' when possible, but this is not
  -- always so, e.g. prefix matching.
  --
  -- XXX: It would be more consistent to denote this as an 'Action', e.g.
  -- > AWS m => a -> Action m (Rs (Rq a)) [b]
  -- with the added advantage of terser definitions; '= EC2.dvrsVPCs' etc.
  candidates :: AWS m => a -> Rs (Rq a) -> m [b]

  -- Validate whether the remote resource precisely corresponds to the local
  -- resource/infra.
  hashesTo :: a -> (b -> Bool)

  -- Given a remote resource, extract its identifier.
  extractId :: Traversal' b ResourceId

data MatchResult = OnwardWith ResourceId
                 | NeedsUpdate ResourceId

data DiscoveryError = NotFound | Ambiguous [Text] deriving Show

toDiscovery :: [Text] -> Either DiscoveryError Text
toDiscovery = \case
  [one] -> Right one
  []    -> Left NotFound
  many  -> Left (Ambiguous many)

gimme :: (AWS m, AWSPager rq) => rq -> m [Rs rq]
gimme = either (throwM :: MonadThrow m => Error -> m a) return
    <=< runConduit . tryC . flip fuse consume . paginate

match' :: AWS m => AWSPager (Rq a) => Matcher a b => a -> m (Either DiscoveryError MatchResult)
match' infra = do
  infras <- infra ^! request >>= gimme >>= fmap concat . mapM (candidates infra)
  return $ case filter (hashesTo infra) infras of
    [one] -> Right . OnwardWith $ one ^. extractId
    []    -> fmap NeedsUpdate . toDiscovery $ view extractId <$> infras
    many  -> Left . Ambiguous $ view extractId <$> many

data Missing = Missing { unMissing :: Reference }

class Resource a where
  -- Search for an existing copy of the resource. Interpretation:
  --
  --  Right (OnwardWith (id :: ResourceId))
  --    ~ A candidate resource was identified, and does not require 'update'.
  --  Right (NeedsUpdate (id :: ResourceId))
  --    ~ A single candidate resource was identified, but it needs to be
  --      'update'd to match the resource.
  --  Left (Ambiguous (xs :: [ResourceId])
  --    ~ Multiple candidate resources were identified.
  --  Left NotFound
  --    ~ No existing version of the resource was identified.
  --
  -- XXX: It would be more informative to disambiguate 'Ambiguous' results
  -- with and without matching hashes.
  match  :: AWS m => a -> m (Either DiscoveryError MatchResult)
  default match :: (AWS m, AWSPager (Rq a), Matcher a b) => a -> m (Either DiscoveryError MatchResult)
  match = match'

  -- Create the resource, yield its identifier.
  create :: AWS m => a -> m ResourceId

  -- Update an identified resource.
  update :: AWS m => ResourceId -> a -> m ResourceId

  -- Substitute any 'RefLocal' references in 'a' for 'RefRemote', using some
  -- correspondence between namespaced 'RefLocal' and 'ResourceId's. If some
  -- local reference cannot be resolved; Left (Missing ~ Reference).
  reify  :: Map Reference ResourceId -> (a -> Either Missing a)
  reify = const return

instance AWSPager EC2.DescribeVPCs where page _ _ = Nothing

hashOf :: Hashable h => h -> Text
hashOf = pack . show . hash

isTag :: Text -> Text -> (EC2.Tag -> Bool)
isTag key value tag = (tag ^. EC2.tagKey) == key && (tag ^. EC2.tagValue) == value

hashTagIs = isTag "hash" . hashOf -- # yolo

_filter :: Getter (Text, Text) EC2.Filter
_filter = \f t@(k, v) -> const t <$> f (EC2.filter' ("tag:" <> k) & EC2.fValues .~ [v])

raise :: Functor f => Getter s a -> Getter (f s) (f a) -- XXX: Lawful?
raise l = to . fmap $ view l

instance Matcher Ec2vpc EC2.VPC where
  type Rq Ec2vpc = EC2.DescribeVPCs
  request = act (const $ asks ctxTags) . raise _filter . to (set EC2.dvsFilters ?? EC2.describeVPCs)
  candidates _ = return . view EC2.dvrsVPCs
  hashesTo vpc = any (hashTagIs vpc) . view EC2.vpcTags
  extractId = EC2.vpcVPCId

defer :: AWS m => AWST (ResourceT IO) a -> m ReleaseKey
defer act = asks ctxEnv >>= liftResourceT . register . void . runResourceT . flip runAWST act

toEc2Tags xs tags =
  void . send $ EC2.createTags & EC2.cResources .~ xs
                               & EC2.cTags .~ map (uncurry EC2.tag) tags

-- "interface EC2APIAttributeServiceValuesFactoryFactory" -- @vlad
true = Just (EC2.attributeBooleanValue & EC2.abvValue ?~ True)

internetAccess defTags vpcId = do
  void . send $ EC2.modifyVPCAttribute vpcId
    & EC2.mvaEnableDNSHostnames .~ true
  void . send $ EC2.modifyVPCAttribute vpcId
    & EC2.mvaEnableDNSSupport .~ true
  Just igw <- view EC2.cigrsInternetGateway <$> send EC2.createInternetGateway
  let gwId = igw ^. EC2.igInternetGatewayId
  void . send $ EC2.attachInternetGateway gwId vpcId
  toEc2Tags [gwId] defTags
  rtb:_rtbs <- view EC2.drtrsRouteTables <$>
          send (EC2.describeRouteTables & EC2.drtsFilters .~ [EC2.filter' "tag:vpc-id" & EC2.fValues .~ [vpcId]])
  let Just rtbId = rtb ^. EC2.rtRouteTableId
  void . send $ EC2.createRoute rtbId "0.0.0.0/0" & EC2.crGatewayId ?~ gwId
  toEc2Tags [rtbId] defTags

instance Resource Ec2vpc where
  create :: AWS m => Ec2vpc -> m ResourceId
  create Ec2vpc{..} = do
    tags <- asks ctxTags
    vpc <- view (EC2.cvrsVPC . _Just . EC2.vpcVPCId) <$> send (EC2.createVPC ec2vpc_cidrBlock)
    await EC2.vpcAvailable (EC2.describeVPCs & EC2.dvsVPCIds .~ [vpc])
    toEc2Tags [vpc] $ ("hash", hashOf Ec2vpc{..}) : tags
    vpc <$ internetAccess tags vpc
  update :: AWS m => ResourceId -> Ec2vpc -> m ResourceId
  update current vpc = do -- XXX: Do something about the internet gateway.
    vpc <- create vpc
    EC2.deleteVPC current & defer.send
    return vpc

instance AWSPager EC2.DescribeSecurityGroups where page _ _ = Nothing

instance Matcher Ec2sg EC2.SecurityGroup where
  type Rq Ec2sg = EC2.DescribeSecurityGroups
  request = act (const $ asks ctxTags) . raise _filter . to (set EC2.dsgsFilters ?? EC2.describeSecurityGroups)
  candidates Ec2sg{..} = fmap return . toListOf $ EC2.dsgrsSecurityGroups
                                                . folded
                                                . filtered (T.isPrefixOf ec2sg_name . view EC2.sgGroupName)
  hashesTo group = any (isTag "hash" $ hashOf group) . view EC2.sgTags
  extractId = EC2.sgGroupId

lookup_ :: Map Reference ResourceId -> (Text -> Reference) -> InfraRef a -> Either Missing (InfraRef a)
lookup_ ledger report = \case
  RefLocal n -> case Map.lookup (report n) ledger of
    Just ref -> Right $ RefRemote ref
    Nothing  -> Left . Missing $ report n
  r@(RefRemote _) -> Right r

instance Resource Ec2sg where
  create :: AWS m => Ec2sg -> m ResourceId
  create Ec2sg{..} = do
    let name = ec2sg_name <> (if T.null ec2sg_name then "" else "-") <> hashOf Ec2sg{..}
    group <- EC2.createSecurityGroup name ec2sg_description
           & case ec2sg_vpc of { Just (RefRemote vpc) -> EC2.csgVPCId ?~ vpc; Nothing -> id; }
           & send <&> view EC2.csgrsGroupId
    asks ctxTags >>= toEc2Tags [group] . (("hash", hashOf Ec2sg{..}):)
    return group
  update :: AWS m => ResourceId -> Ec2sg -> m ResourceId
  update current group = do
    group <- create group
    EC2.deleteSecurityGroup & EC2.dsgGroupId ?~ current & defer . send
    return group
  reify :: Map Reference ResourceId -> (Ec2sg -> Either Missing Ec2sg)
  reify (lookup_ -> lookup_) Ec2sg{..} = do
    ec2sg_vpc <- forM ec2sg_vpc $ lookup_ (Reference "ec2-vpc")
    return Ec2sg{..}

toPermission :: Rule -> EC2.IPPermission
toPermission Rule{..} = EC2.ipPermission rule_protocol
                          & EC2.ipFromPort .~ (fromIntegral <$> rule_fromPort)
                          & EC2.ipToPort .~ (fromIntegral <$> rule_toPort)
                          & EC2.ipIPRanges .~ (EC2.ipRange <$> maybeToList rule_sourceIp)

instance Resource Ec2sgruleset where
  match :: AWS m => Ec2sgruleset -> m (Either DiscoveryError MatchResult)
  match = const . return . Right $ NeedsUpdate "(virtual)"
  create :: AWS m => Ec2sgruleset -> m ResourceId
  create Ec2sgruleset{..} = "(virtual)" <$ do
    EC2.authorizeSecurityGroupIngress
      & EC2.asgiGroupId ?~ fromRefRemote ec2sgruleset_securityGroup
      & EC2.asgiIPPermissions .~ map toPermission ec2sgruleset_rules
      & unless (null ec2sgruleset_rules) . void . send
  update :: AWS m => ResourceId -> Ec2sgruleset -> m ResourceId
  update _ Ec2sgruleset{..} = "(virtual)" <$ do
    currentRules <- EC2.describeSecurityGroups
      & EC2.dsgsGroupIds .~ [fromRefRemote ec2sgruleset_securityGroup]
      & send <&> view (EC2.dsgrsSecurityGroups . folded . EC2.sgIPPermissions)
    let intendedRules = map toPermission ec2sgruleset_rules
        (aight, drop) = partition (`elem` intendedRules) currentRules
        authorizeThem = filter (`notElem` aight) intendedRules
    EC2.authorizeSecurityGroupIngress
      & EC2.asgiGroupId ?~ fromRefRemote ec2sgruleset_securityGroup
      & EC2.asgiIPPermissions .~ authorizeThem
      & unless (null authorizeThem) . void . send
    EC2.revokeSecurityGroupIngress
      & EC2.rsgiGroupId ?~ fromRefRemote ec2sgruleset_securityGroup
      & EC2.rsgiIPPermissions .~ drop
      & unless (null drop) . void . send
  reify :: Map Reference ResourceId -> (Ec2sgruleset -> Either Missing Ec2sgruleset)
  reify (lookup_ -> lookup_) Ec2sgruleset{..} = do
    ec2sgruleset_securityGroup <- lookup_ (Reference "ec2-sg") ec2sgruleset_securityGroup
    return Ec2sgruleset{..}

instance AWSPager EC2.DescribeVolumes where page _ _ = Nothing

instance Matcher Ebs EC2.Volume where
  type Rq Ebs = EC2.DescribeVolumes
  request = act (\Ebs{..} -> cons ("Name", ebs_name) <$> asks ctxTags)
          . raise _filter . to (set EC2.desFilters ?? EC2.describeVolumes)
  candidates _ = return . view EC2.dvvrsVolumes
  hashesTo ebs = any (isTag "hash" $ hashOf ebs) . view EC2.vTags
  extractId = EC2.vVolumeId

toVolumeType Gp2 = EC2.GP2
toVolumeType (Iop _) = EC2.IO1
toVolumeType Standard = EC2.Standard

instance Resource Ebs where
  create :: AWS m => Ebs -> m ResourceId
  create Ebs{..} = do
    tags <- asks ctxTags <&> (("hash", hashOf Ebs{..}):) . (("Name", ebs_name):)
    ebs <- EC2.createVolume ebs_zone
         & EC2.creSnapshotId .~ ebs_snapshot
         & EC2.creVolumeType ?~ toVolumeType ebs_volumeType
         & EC2.creIOPS .~ (case ebs_volumeType of Iop n -> Just $ fromIntegral n; _ -> Nothing)
         & EC2.creSize ?~ fromIntegral ebs_size
         & send <&> view EC2.vVolumeId
    ebs <$ toEc2Tags [ebs] tags
  update :: AWS m => ResourceId -> Ebs -> m ResourceId
  update current ebs = do
    snapshot <- view EC2.sSnapshotId <$> send (EC2.createSnapshot current)
    await EC2.snapshotCompleted (EC2.describeSnapshots & EC2.dssSnapshotIds .~ [snapshot])
    volume  <- create $ ebs { ebs_snapshot = Just snapshot }
    await EC2.volumeAvailable (EC2.describeVolumes & EC2.desVolumeIds .~ [volume])
    EC2.deleteSnapshot snapshot & defer . send
    EC2.deleteVolume current & defer . send
    return volume

instance AWSPager EC2.DescribeSubnets where page _ _ = Nothing

instance Matcher Ec2subnet EC2.Subnet where
  type Rq Ec2subnet = EC2.DescribeSubnets
  request = act (\Ec2subnet{..} -> cons ("Name", ec2subnet_name) <$> asks ctxTags)
          . raise _filter . to (set EC2.dsFilters ?? EC2.describeSubnets)
  candidates _ = return . view EC2.dsrsSubnets
  hashesTo subnet = any (isTag "hash" $ hashOf subnet) . view EC2.subTags
  extractId = EC2.subSubnetId

overlaps :: AddrRange IPv4 -> AddrRange IPv4 -> Bool
a `overlaps` b = a >:> b || b >:> a

fromRefRemote :: InfraRef a -> ResourceId
fromRefRemote (RefRemote r) = r

instance Resource Ec2subnet where
  create :: AWS m => Ec2subnet -> m ResourceId
  create Ec2subnet{..} = do
    subnet <- EC2.createSubnet (fromRefRemote ec2subnet_vpc) ec2subnet_cidrBlock
            & EC2.cssAvailabilityZone ?~ ec2subnet_zone
            & send <&> view (EC2.crsSubnet . _Just . EC2.subSubnetId)
    asks ctxTags >>= toEc2Tags [subnet] . (("hash", hashOf Ec2subnet{..}):) . (("Name", ec2subnet_name):)
    return subnet
  update :: AWS m => ResourceId -> Ec2subnet -> m ResourceId
  update current Ec2subnet{..} = do
    -- If CIDRs overlap, we needs to delete *now* to avoid conflict on 'create'.
    currentCIDR <- EC2.describeSubnets
                 & EC2.dsFilters .~ [EC2.filter' "subnet-id" & EC2.fValues .~ [current]]
                 & send <&> read . unpack . view (EC2.dsrsSubnets . folded . EC2.subCIdRBlock)
    let canDefer = currentCIDR `overlaps` read (unpack ec2subnet_cidrBlock)
    EC2.deleteSubnet current & (if canDefer then void . defer else void . return) . send
    create Ec2subnet{..}
  reify :: Map Reference ResourceId -> Ec2subnet -> Either Missing Ec2subnet
  reify (lookup_ -> lookup_) Ec2subnet{..} = do
    ec2subnet_vpc <- lookup_ (Reference "ec2-vpc") ec2subnet_vpc
    return Ec2subnet{..}

keyPrefix :: Tags -> Ec2keypair -> Text
keyPrefix tags Ec2keypair{..} = T.intercalate "-" $
  [ tags ! "created-using"
  , tags ! "realm"
  , tags ! "expression"
  , ec2keypair_name
  ] where (!) = fmap (fromMaybe "") . flip lookup

instance Matcher Ec2keypair EC2.KeyPairInfo where
  type Rq Ec2keypair = EC2.DescribeKeyPairs
  request = \f s -> const s <$> f EC2.describeKeyPairs
  candidates Ec2keypair{..} keypairs =  do
    prefix <- flip keyPrefix Ec2keypair{..} <$> asks ctxTags
    keypairs ^.. EC2.dkprsKeyPairs . folded . filtered (T.isPrefixOf prefix . view extractId) & return
  hashesTo keypair = T.isSuffixOf (hashOf keypair) . view (EC2.kpiKeyName . _Just)
  -- XXX: If 'Nothing', "" by mempty on Traversal, don't know how bad this is.
  extractId = EC2.kpiKeyName . _Just

instance AWSPager EC2.DescribeKeyPairs where page _ _ = Nothing

addKeypair :: AWS m => Ec2keypair -> m ()
addKeypair key = modify $ \State{..} -> State{..} { stateKeyPairs = key : stateKeyPairs }

instance Resource Ec2keypair where
  create :: AWS m => Ec2keypair -> m ResourceId
  create Ec2keypair{..} = do
    name <- (<> "-" <> hashOf Ec2keypair{..}) . flip keyPrefix Ec2keypair{..} <$> asks ctxTags
    -- XXX: This filename interface leads to bad hashing; contents can change without updates registering.
    pubkey <- liftIO . expectRight . fgconsume $ exec "ssh-keygen" ["-f", unpack ec2keypair_privateKeyFile, "-y"]
    EC2.importKeyPair name pubkey & send <&> view (EC2.ikprsKeyName . _Just)
  update :: AWS m => ResourceId -> Ec2keypair -> m ResourceId
  update current Ec2keypair{..} = do
    keypair <- create Ec2keypair{..}
    EC2.deleteKeyPair current & defer . send
    return keypair
  match Ec2keypair{..} = do
    prefix <- asks ctxTags <&> flip keyPrefix Ec2keypair{..}
    addKeypair $ Ec2keypair{..} { ec2keypair_name = prefix <> "-" <> hashOf Ec2keypair{..} }
    match' Ec2keypair{..}

toInstanceType :: Text -> EC2.InstanceType
toInstanceType instanceType = case AWS.fromText instanceType of
  Left e -> error $ "Unknown instance type: " ++ show instanceType
  Right r -> r

append :: a -> Lens' [a] [a] -- XXX: Unlawful, be careful.
append a = \f s -> init <$> f (a:s)

instance Matcher Ec2instance EC2.Instance where
  type Rq Ec2instance = EC2.DescribeInstances
  request = act (\Ec2instance{..} -> cons ("Name", ec2instance_name) <$> asks ctxTags)
          . raise _filter
          . append (EC2.filter' "instance-state-name" & EC2.fValues .~ ["pending", "running", "stopped"])
          . to (set EC2.diiFilters ?? EC2.describeInstances)
  candidates _ reservations = reservations ^. EC2.dirsReservations . folded . EC2.rInstances & return
  hashesTo inst = any (isTag "hash" $ hashOf inst) . view EC2.insTags
  extractId = EC2.insInstanceId

addInstance :: AWS m => EC2.Instance -> m ()
addInstance inst = modify $ \State{..} -> State{..} { stateInstances = inst : stateInstances }

xformMapping :: BlockDeviceMapping -> Maybe EC2.BlockDeviceMapping
xformMapping BlockDeviceMapping{..} =
  case blockDeviceMapping_disk of
   RefLocal _ -> fail "ignore (handled later)"
   RefRemote x | "ephemeral" `T.isPrefixOf` x ->
                 return (EC2.blockDeviceMapping blockDeviceMapping_blockDeviceMappingName
                         & EC2.bdmVirtualName ?~ x)
               | otherwise ->
                 fail "ignore (not implemented)"

instance Resource Ec2instance where
  create :: AWS m => Ec2instance -> m ResourceId
  create Ec2instance{..} = do
    udata <- mapM (fmap String . T.readFile . unpack) ec2instance_userData
         <&> insert "hostname" (String ec2instance_name)
         <&> decodeUtf8 . Base64.encode . toStrict . encode . object . toList
           & liftIO
    inst <- EC2.runInstances ec2instance_ami 1 1
          & EC2.rInstanceType ?~ toInstanceType ec2instance_instanceType
          & EC2.rUserData ?~ udata
          & EC2.rKeyName ?~ fromRefRemote ec2instance_keyPair
          & EC2.rEBSOptimized ?~ ec2instance_ebsOptimized
          & EC2.rPlacement ?~ (EC2.placement & EC2.pAvailabilityZone ?~ ec2instance_zone)
          & EC2.rIAMInstanceProfile ?~ ( EC2.iamInstanceProfileSpecification
                                       & EC2.iapsARN .~ ec2instance_instanceProfileARN)
          & EC2.rNetworkInterfaces .~ [ EC2.instanceNetworkInterfaceSpecification
                                      & EC2.inisAssociatePublicIPAddress ?~ True
                                      & EC2.inisSubnetId .~ fmap fromRefRemote ec2instance_subnet
                                      & EC2.inisGroups .~ map fromRefRemote ec2instance_securityGroups
                                      & EC2.inisDeviceIndex ?~ 0
                                      ]
          & EC2.rMonitoring ?~ EC2.runInstancesMonitoringEnabled True
          & EC2.rBlockDeviceMappings .~ (catMaybes . elems . fmap xformMapping $ ec2instance_blockDeviceMapping)
          & send <&> view (EC2.rInstances . to head)
        >>= (\i -> addInstance i >> return (i ^. EC2.insInstanceId))
    asks ctxTags >>= toEc2Tags [inst] . (("Name", ec2instance_name):) . (("hash", hashOf Ec2instance{..}):)
    await EC2.instanceRunning (EC2.describeInstances & EC2.diiInstanceIds .~ [inst])
    forM ec2instance_blockDeviceMapping $ \BlockDeviceMapping{..} -> do
      let RefRemote volume = blockDeviceMapping_disk
      maybe (return ()) (void . send) $ if
        | T.isPrefixOf "vol-" volume -> Just $
            EC2.attachVolume volume inst blockDeviceMapping_blockDeviceMappingName
        | T.isPrefixOf "ephemeral" volume -> Nothing
        | otherwise -> error $ "Don't know how to handle volume " ++ show volume
    return inst
  update :: AWS m => ResourceId -> Ec2instance -> m ResourceId
  update current inst = do
    currentVolumes <- EC2.describeInstances & EC2.diiInstanceIds .~ [current] & send <&> toListOf
      ( EC2.dirsReservations . folded
      . EC2.rInstances . folded
      . EC2.insBlockDeviceMappings . folded
      . EC2.ibdmEBS . folded . filtered ((/= Just True) . view EC2.eibdDeleteOnTermination)
      . EC2.eibdVolumeId . _Just
      )
    mapM (send . EC2.detachVolume) currentVolumes
    unless (null currentVolumes) $
      await EC2.volumeAvailable (EC2.describeVolumes & EC2.desVolumeIds .~ currentVolumes)
    EC2.terminateInstances & EC2.tiInstanceIds .~ [current] & defer . send
    create inst
  reify :: Map Reference ResourceId -> (Ec2instance -> Either Missing Ec2instance)
  reify ledger Ec2instance{..} = do
    ec2instance_blockDeviceMapping <- forM ec2instance_blockDeviceMapping $ \BlockDeviceMapping{..} -> do
      blockDeviceMapping_disk <- lookup_ ledger (Reference "ebs") blockDeviceMapping_disk
      return BlockDeviceMapping{..}
    ec2instance_keyPair <- lookup_ ledger (Reference "ec2-keypair") ec2instance_keyPair
    ec2instance_securityGroups <- lookup_ ledger (Reference "ec2-sg") `mapM` ec2instance_securityGroups
    ec2instance_subnet <- lookup_ ledger (Reference "ec2-subnet") `mapM` ec2instance_subnet
    return Ec2instance{..}
  match infra = do
    infras <- infra ^! request >>= gimme >>= fmap concat . mapM (candidates infra)
    case filter (hashesTo infra) infras of
      []    -> return . fmap NeedsUpdate . toDiscovery $ view extractId <$> infras
      -- XXX: Registering instances here is overly assumptive of
      -- downstream semantics, but I don't have a better idea yet.
      -- Specifically, we assume that if a matching instance is
      -- found, then 'create' or 'update' will never fire.
      [one] -> Right (OnwardWith $ one ^. extractId) <$ addInstance one
      many  -> return . Left . Ambiguous $ view extractId <$> many

instance Matcher Elb (Tags, ELB.LoadBalancerDescription) where
  type Rq Elb = ELB.DescribeLoadBalancers
  request = \f s -> const s <$> f ELB.describeLoadBalancers
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
  -- XXX: If '(_, Nothing)', "" by mempty on Traversal, don't know how bad this is.
  extractId = _2 . ELB.lbdLoadBalancerName . _Just

toElbTag :: (Text, Text) -> ELB.Tag
toElbTag = \(k, v) -> ELB.tag k & ELB.tagValue ?~ v

toElbListener :: Listener -> ELB.Listener
toElbListener Listener{..} = ELB.listener listener_lbProtocol
                            (fromIntegral listener_lbPort)
                            (fromIntegral listener_instancePort)
                        & ELB.lInstanceProtocol ?~ listener_instanceProtocol
                        & ELB.lSSLCertificateId .~ case listener_sslCertificateId of { "" -> Nothing; x -> Just x }

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

toElbHealthcheck :: HealthCheck -> ELB.HealthCheck
toElbHealthcheck HealthCheck{..} = ELB.healthCheck
  (toElbHealthcheckTarget healthCheck_target)
  (fromIntegral healthCheck_interval)
  (fromIntegral healthCheck_timeout)
  (fromIntegral healthCheck_unhealthyThreshold)
  (fromIntegral healthCheck_healthyThreshold)

toElbInstance :: InfraRef Ec2instance -> ELB.Instance
toElbInstance ref = ELB.instance' & ELB.iInstanceId .~ case ref of
  RefRemote r -> Just r
  RefLocal  _ -> Nothing

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

instance Resource Elb where
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
    unless (null elb_instances) $ ELB.registerInstancesWithLoadBalancer elb_name'
      & ELB.riwlbInstances .~ map toElbInstance elb_instances
      & void . send
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
    elb_instances <- lookup_ ledger (Reference "ec2-instance") `mapM` elb_instances
    elb_securityGroups <- lookup_ ledger (Reference "ec2-sg") `mapM` elb_securityGroups
    elb_subnets <- lookup_ ledger (Reference "ec2-subnet") `mapM` elb_subnets
    return Elb{..}
