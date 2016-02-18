{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RecordWildCards        #-}

module Upcast.Infra.Resources.VPC where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Control.Lens hiding (Context) -- (*)
import Control.Lens.Action
import Control.Monad.Trans.AWS (send, await)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad (void)
import Data.Map (Map)

import Data.Text (Text, unpack)

import qualified Network.AWS.EC2 as EC2 -- (*)

import Upcast.Infra.Types.Amazonka -- (*)
import Upcast.Infra.NixTypes -- (*)
import Upcast.Infra.Types.Common

-- *

request :: AWS m => Action m Ec2vpc EC2.DescribeVPCs
request = act (const $ asks ctxTags) . raise _filter . to (set EC2.dvsFilters ?? EC2.describeVPCs)

candidates _ = return . view EC2.dvrsVPCs

hashesTo vpc = any (hashTagIs vpc) . view EC2.vpcTags

extractId :: Traversal' EC2.VPC ResourceId
extractId = EC2.vpcVPCId


-- *

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

-- *

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
