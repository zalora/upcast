
module Upcast.Aws (
  instances
, inst
) where

import qualified Aws
import qualified Aws.S3 as S3
import Aws.Ec2.Core

import Data.Conduit (($$+-))
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit (withManager, responseBody)

import qualified Aws.Ec2.Commands.DescribeInstances as DI

instances = undefined

inst :: IO DI.DescribeInstancesResponse
inst = do
  cfg <- Aws.dbgConfiguration
  let ec2cfg = Aws.defServiceConfig :: EC2Configuration Aws.NormalQuery

  r <- Aws.simpleAws cfg ec2cfg DI.DescribeInstances
  return r
