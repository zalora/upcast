
module Upcast.Aws where

import Control.Monad
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson.Encode.Pretty

import qualified Aws
import Aws.Ec2.Core
import Aws.Ec2.Types

import qualified Aws.Ec2.Commands.DescribeInstances as DI
import qualified Aws.Ec2.Commands.DescribeVpcs as DV
import qualified Aws.Ec2.Commands.CreateVpc as CV
import qualified Aws.Ec2.Commands.DescribeSubnets as DS
import qualified Aws.Ec2.Commands.CreateSubnet as CS

simpleAws arg region = do
    -- cfg <- Aws.dbgConfiguration
    cfg <- Aws.baseConfiguration
    Aws.simpleAws cfg (EC2Configuration region) arg

pe = LBS.putStrLn . encodePretty

instances = pe <=< simpleAws DI.DescribeInstances
vpcs = pe <=< simpleAws DV.DescribeVpcs
createVpc block = pe <=< simpleAws (CV.CreateVpc block CV.Default)
subnets = pe <=< simpleAws DS.DescribeSubnets
createSubnet vpc block = pe <=< simpleAws (CS.CreateSubnet vpc block)
