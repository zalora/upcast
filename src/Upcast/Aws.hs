
module Upcast.Aws where

import Control.Monad
import Control.Concurrent.Async
import qualified Control.Exception
import Data.Either

import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson.Types (ToJSON)
import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)

import qualified Aws
import Aws.Ec2.Core
import Aws.Ec2.Types

import qualified Aws.Ec2.Commands.DescribeAvailabilityZones as EC2
import qualified Aws.Ec2.Commands.DescribeInstances as EC2
import qualified Aws.Ec2.Commands.DescribeImages as EC2

import qualified Aws.Ec2.Commands.DescribeVpcs as EC2
import qualified Aws.Ec2.Commands.CreateVpc as EC2

import qualified Aws.Ec2.Commands.DescribeSubnets as EC2
import qualified Aws.Ec2.Commands.CreateSubnet as EC2

import qualified Aws.Ec2.Commands.DescribeVolumes as EC2
import qualified Aws.Ec2.Commands.DescribeVolumeStatus as EC2
import qualified Aws.Ec2.Commands.CreateVolume as EC2
import qualified Aws.Ec2.Commands.AttachVolume as EC2

catchAny :: IO a -> (Control.Exception.SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

simpleAws arg region = do
    -- cfg <- Aws.dbgConfiguration
    cfg <- Aws.baseConfiguration
    Aws.simpleAws cfg (EC2Configuration region) arg

pprint :: ToJSON a => a -> IO ()
pprint = LBS.putStrLn . encodePretty

instances :: B.ByteString -> IO Value
instances = simpleAws EC2.DescribeInstances

vpcs :: B.ByteString -> IO Value
vpcs = simpleAws EC2.DescribeVpcs

createVpc :: Text -> B.ByteString -> IO Value
createVpc block = simpleAws (EC2.CreateVpc block EC2.Default)

subnets :: B.ByteString -> IO Value
subnets = simpleAws EC2.DescribeSubnets

createSubnet :: Text -> Text -> B.ByteString -> IO Value
createSubnet vpc block = simpleAws (EC2.CreateSubnet vpc block)

volumes :: B.ByteString -> IO Value
volumes = simpleAws EC2.DescribeVolumes

volumeStatus :: B.ByteString -> IO Value
volumeStatus = simpleAws EC2.DescribeVolumeStatus

azs :: B.ByteString -> IO Value
azs = simpleAws EC2.DescribeAvailabilityZones

images :: B.ByteString -> IO Value
images = simpleAws EC2.DescribeImages

regions :: [B.ByteString]
regions = [ "ap-northeast-1"
          , "ap-southeast-1"
          , "ap-southeast-2"
          , "eu-west-1"
          , "eu-central-1" -- coming soon!
          , "sa-east-1"
          , "us-east-1"
          , "us-west-1"
          , "us-west-2"
          ]

status = mapConcurrently ((flip catchAny $ \e -> return $ Left e) . fmap Right . azs) regions
pstatus = fmap rights status >>= mapM_ (LBS.putStrLn . encode)
