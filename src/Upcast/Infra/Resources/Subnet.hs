{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ViewPatterns           #-}

module Upcast.Infra.Resources.Subnet where

import Control.Applicative -- (*)
import Control.Monad.Trans (liftIO)
import Control.Lens hiding (Context) -- (*)
import Control.Lens.Action -- (*)
import Control.Monad.Trans.AWS (send)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad (void)
import Data.Map (Map)

import Data.IP ((>:>), AddrRange(..), IPv4)

import Data.Text (Text, unpack)

import qualified Network.AWS.EC2 as EC2 -- (*)

import Upcast.Infra.Types.Amazonka -- (*)
import Upcast.Infra.NixTypes -- (*)
import Upcast.Infra.Types.Common -- (*)

-- *

request :: AWS m => Action m Ec2subnet EC2.DescribeSubnets
request = act (\Ec2subnet{..} -> cons ("Name", ec2subnet_name) <$> asks ctxTags)
              . raise _filter . to (set EC2.dsFilters ?? EC2.describeSubnets)

candidates _ = return . view EC2.dsrsSubnets
hashesTo subnet = any (isTag "hash" $ hashOf subnet) . view EC2.subTags

extractId :: Traversal' EC2.Subnet ResourceId
extractId = EC2.subSubnetId

-- *

overlaps :: AddrRange IPv4 -> AddrRange IPv4 -> Bool
a `overlaps` b = a >:> b || b >:> a

-- *

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
