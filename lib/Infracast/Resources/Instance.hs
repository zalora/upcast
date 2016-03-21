{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE LambdaCase             #-}

module Infracast.Resources.Instance where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Control.Lens hiding (Context) -- (*)
import Control.Lens.Action
import Control.Monad.Trans.AWS (Rs, AWSPager(..), send, await, paginate, AWST, runAWST)
import Control.Monad.State (modify)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad (void)

import qualified Data.ByteString.Base64 as Base64 (encode)
import Data.ByteString.Lazy (toStrict)
import Data.Aeson (encode)
import Data.Aeson.Lens () -- Plated Value
import Data.Aeson.Types (Value(String), object)
import Data.Maybe (catMaybes)
import Data.Traversable (mapM, forM)

import Data.Map (Map, toList, insert, elems)
import qualified Data.Map as Map (lookup)

import qualified Data.Text as T (null, isPrefixOf, isSuffixOf, intercalate, take)
import qualified Data.Text.IO as T (readFile)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)

import Network.AWS.Waiter (Wait(_waitAttempts), Accept(..))
import qualified Network.AWS.Data as AWS (fromText)
import qualified Network.AWS.EC2 as EC2 -- (*)
import qualified Network.AWS.CloudWatch as CW -- (*)

import Infracast.Amazonka -- (*)
import Infracast.NixTypes -- (*)
import Infracast.Types -- (*)

-- * Matcher

request :: AWS m => Action m Ec2instance EC2.DescribeInstances
request = act (\Ec2instance{..} -> cons ("Name", ec2instance_name) <$> asks ctxTags)
        . raise _filter
        . append (EC2.filter' "instance-state-name" & EC2.fValues .~ ["pending", "running", "stopped"])
        . to (set EC2.diiFilters ?? EC2.describeInstances)

candidates _ reservations = reservations ^. EC2.dirsReservations . folded . EC2.rInstances & return

hashesTo inst = any (isTag "hash" $ hashOf inst) . view EC2.insTags

extractId :: Traversal' EC2.Instance ResourceId
extractId = EC2.insInstanceId

-- *

toInstanceType :: Text -> EC2.InstanceType
toInstanceType instanceType = case AWS.fromText instanceType of
  Left e -> error $ "Unknown instance type: " ++ show instanceType
  Right r -> r

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

-- * instance Resource Ec2instance where

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
  forM currentVolumes $ \volume -> do
    EC2.detachVolume volume & send
    let describeVolumes = EC2.describeVolumes & EC2.desVolumeIds .~ [volume]
        waiter = await (EC2.volumeAvailable { _waitAttempts = 5 }) describeVolumes
    waiter >>= \case { AcceptSuccess -> return (); _ -> do
      EC2.detachVolume volume & EC2.dvForce ?~ True & send
      AcceptSuccess <- waiter
      return ()
    }
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

match :: AWS m => Ec2instance -> m (Either DiscoveryError MatchResult)
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
