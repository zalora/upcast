{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards #-}

module Upcast.Resource where

import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map

import Upcast.Interpolate (n)

import Upcast.State
import Upcast.Nix
import Upcast.Command
import Upcast.Temp
import Upcast.Aws
import Upcast.ATerm (alookupS)
import Upcast.DeployCommands
import Upcast.Deploy

resourcePlan exprFile ctx@DeployContext{..} = do
    let s = emptyState exprFile
    Right info <- deploymentInfo ctx s

    let Just vpcs = alookupS "resources.vpc" info
    let Just subnets = alookupS "resources.subnets" info
    let Just volumes = alookupS "resources.ebsVolumes" info
    let Just keypairs = alookupS "resources.ec2KeyPairs" info
    let Just secGroups = alookupS "resources.ec2SecurityGroups" info
    -- let Just instances = castValue $ alookupS "resources.machines" :: Maybe Map
    return [ vpcs
           , subnets
           , volumes
           , keypairs
           , secGroups
           ]

