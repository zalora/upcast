
module Upcast.Aws where

import Control.Monad
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson.Encode.Pretty

import qualified Aws
import Aws.Ec2.Core
import Aws.Ec2.Types

import qualified Aws.Ec2.Commands.DescribeInstances as EC2

import qualified Aws.Ec2.Commands.DescribeVpcs as EC2
import qualified Aws.Ec2.Commands.CreateVpc as EC2

import qualified Aws.Ec2.Commands.DescribeSubnets as EC2
import qualified Aws.Ec2.Commands.CreateSubnet as EC2

import qualified Aws.Ec2.Commands.DescribeVolumes as EC2

simpleAws arg region = do
    -- cfg <- Aws.dbgConfiguration
    cfg <- Aws.baseConfiguration
    Aws.simpleAws cfg (EC2Configuration region) arg

pe = LBS.putStrLn . encodePretty

instances = pe <=< simpleAws EC2.DescribeInstances

vpcs = pe <=< simpleAws EC2.DescribeVpcs
createVpc block = pe <=< simpleAws (EC2.CreateVpc block EC2.Default)

subnets = pe <=< simpleAws EC2.DescribeSubnets
createSubnet vpc block = pe <=< simpleAws (EC2.CreateSubnet vpc block)

volumes = pe <=< simpleAws EC2.DescribeVolumes

