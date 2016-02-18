{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ViewPatterns           #-}

module Upcast.Infra.Resources.SecurityGroup where

import Control.Applicative -- (*)
import Control.Monad.Trans (liftIO)
import Control.Lens hiding (Context) -- (*)
import Control.Lens.Action -- (*)
import Control.Monad.Trans.AWS (send)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad (void, unless)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Maybe (maybeToList)
import Data.Traversable (forM)

import Data.Text (Text, unpack)
import qualified Data.Text as T (isPrefixOf, null)

import qualified Network.AWS.EC2 as EC2 -- (*)

import Upcast.Infra.Amazonka -- (*)
import Upcast.Infra.NixTypes -- (*)
import Upcast.Infra.Types -- (*)

-- *

request :: AWS m => Action m Ec2sg EC2.DescribeSecurityGroups
request = act (const $ asks ctxTags) . raise _filter . to (set EC2.dsgsFilters ?? EC2.describeSecurityGroups)

candidates Ec2sg{..} = fmap return . toListOf $ EC2.dsgrsSecurityGroups
                                              . folded
                                              . filtered (T.isPrefixOf ec2sg_name . view EC2.sgGroupName)

hashesTo group = any (isTag "hash" $ hashOf group) . view EC2.sgTags

extractId :: Traversal' EC2.SecurityGroup ResourceId
extractId = EC2.sgGroupId

-- *

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

-- *

-- If permission's EC2.ipIPRanges contains multiple IP ranges,
-- it will be break down into list of same permissions with a single
-- IP range each, for correct diff in the 'converge' function.
singlifyIPRanges :: EC2.IPPermission -> [EC2.IPPermission]
singlifyIPRanges permission =
  let ipRanges = view EC2.ipIPRanges permission in
  if length ipRanges > 1
    then [permission & EC2.ipIPRanges .~ [range] | range <- ipRanges]
    else [permission]


toPermission :: Rule -> EC2.IPPermission
toPermission Rule{..} = EC2.ipPermission rule_protocol
                          & EC2.ipFromPort .~ (fromIntegral <$> rule_fromPort)
                          & EC2.ipToPort .~ (fromIntegral <$> rule_toPort)
                          & EC2.ipIPRanges .~ (EC2.ipRange <$> maybeToList rule_sourceIp)


-- * Ec2sgruleset

matchRuleset :: AWS m => Ec2sgruleset -> m (Either DiscoveryError MatchResult)
matchRuleset = virtual

createRuleset :: AWS m => Ec2sgruleset -> m ResourceId
createRuleset Ec2sgruleset{..} = "(virtual)" <$ do
  EC2.authorizeSecurityGroupIngress
    & EC2.asgiGroupId ?~ fromRefRemote ec2sgruleset_securityGroup
    & EC2.asgiIPPermissions .~ map toPermission ec2sgruleset_rules
    & unless (null ec2sgruleset_rules) . void . send

updateRuleset :: AWS m => ResourceId -> Ec2sgruleset -> m ResourceId
updateRuleset _ Ec2sgruleset{..} = "(virtual)" <$ do
  currentRules <- EC2.describeSecurityGroups
    & EC2.dsgsGroupIds .~ [fromRefRemote ec2sgruleset_securityGroup]
    & send <&> view (EC2.dsgrsSecurityGroups . folded . EC2.sgIPPermissions)
  let currentRulesWithSingleIPRange = concatMap singlifyIPRanges currentRules
      intendedRules = map toPermission ec2sgruleset_rules
      (authorize, revoke) = currentRulesWithSingleIPRange `converge` intendedRules
  EC2.authorizeSecurityGroupIngress
    & EC2.asgiGroupId ?~ fromRefRemote ec2sgruleset_securityGroup
    & EC2.asgiIPPermissions .~ authorize
    & unless (null authorize) . void . send
  EC2.revokeSecurityGroupIngress
    & EC2.rsgiGroupId ?~ fromRefRemote ec2sgruleset_securityGroup
    & EC2.rsgiIPPermissions .~ revoke
    & unless (null revoke) . void . send

reifyRuleset :: Map Reference ResourceId -> (Ec2sgruleset -> Either Missing Ec2sgruleset)
reifyRuleset (lookup_ -> lookup_) Ec2sgruleset{..} = do
  ec2sgruleset_securityGroup <- lookup_ (Reference "ec2-sg") ec2sgruleset_securityGroup
  return Ec2sgruleset{..}
