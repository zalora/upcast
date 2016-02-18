{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE LambdaCase             #-}

module Upcast.Infra.Resources.EBS where

import Control.Applicative -- (*)
import Control.Monad.Trans (liftIO)
import Control.Lens hiding (Context) -- (*)
import Control.Lens.Action
import Control.Monad.Trans.AWS (send, await)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad (void)
import Data.Map (Map)
import Data.Text (Text, unpack)

import qualified Network.AWS.EC2 as EC2 -- (*)

import Upcast.Infra.Amazonka -- (*)
import Upcast.Infra.NixTypes -- (*)
import Upcast.Infra.Types

-- *

request :: AWS m => Action m Ebs EC2.DescribeVolumes
request = act (\Ebs{..} -> cons ("Name", ebs_name) <$> asks ctxTags)
        . raise _filter . to (set EC2.desFilters ?? EC2.describeVolumes)

candidates _ = return . view EC2.dvvrsVolumes
hashesTo ebs = any (isTag "hash" $ hashOf ebs) . view EC2.vTags

extractId :: Traversal' EC2.Volume ResourceId
extractId = EC2.vVolumeId

-- *

toVolumeType = \case
  Gp2 -> EC2.GP2
  (Iop _) -> EC2.IO1
  Standard -> EC2.Standard

-- *

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
