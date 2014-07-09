{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Upcast.Aws (
  simpleAws
, pprint
, instances
, securityGroups
, securityGroupsByName
, console
, pconsole
, vpcs
, createVpc
, subnets
, createSubnet
, volumes
, volumeStatus
, azs
, images
, tags
, keypairs
, regions
, knownRegions
, status
, pstatus
) where

import Control.Monad
import Control.Applicative
import Control.Concurrent.Async
import qualified Control.Exception
import Data.Either
import Data.Maybe

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Base64 as Base64
import Data.Aeson.Types (ToJSON)
import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)

import qualified Aws
import Aws.Core (Transaction)
import Aws.Ec2.Core
import Aws.Ec2.Types
import qualified Aws.Ec2.Info as EC2

import qualified Aws.Ec2.Commands.DescribeAvailabilityZones as EC2
import qualified Aws.Ec2.Commands.DescribeRegions as EC2
import qualified Aws.Ec2.Commands.DescribeImages as EC2

import qualified Aws.Ec2.Commands.DescribeSecurityGroups as EC2
import qualified Aws.Ec2.Commands.DescribeTags as EC2
import qualified Aws.Ec2.Commands.DescribeKeyPairs as EC2
import qualified Aws.Ec2.Commands.ImportKeyPair as EC2

import qualified Aws.Ec2.Commands.DescribeInstances as EC2
import qualified Aws.Ec2.Commands.RunInstances as EC2
import qualified Aws.Ec2.Commands.GetConsoleOutput as EC2

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

type RegionAws = B.ByteString -> IO Value

simpleAws :: (Transaction r Value, Aws.ServiceConfiguration r ~ EC2Configuration) => r -> RegionAws
simpleAws arg region = do
    -- cfg <- Aws.dbgConfiguration
    cfg <- Aws.baseConfiguration
    Aws.simpleAws cfg (EC2Configuration region) arg

pprint :: ToJSON a => a -> IO ()
pprint = LBS.putStrLn . encodePretty

instances = simpleAws EC2.DescribeInstances

securityGroups :: [EC2.SecurityGroupId] -> [EC2.SecurityGroupName] -> RegionAws
securityGroups ids names = simpleAws $ EC2.DescribeSecurityGroups ids names

securityGroupsByName name = securityGroups [] [name]

console :: Text -> B.ByteString -> IO (UTCTime, B.ByteString)
console inst reg = cast <$> simpleAws (EC2.GetConsoleOutput inst) reg
  where
    cast v = let o = out v in (EC2.timestamp o, decode o)
    decode = Base64.decodeLenient . encodeUtf8 . EC2.output
    out = fromJust . (castValue :: Value -> Maybe EC2.ConsoleOutput)

pconsole :: Text -> B.ByteString -> IO ()
pconsole inst reg = do
    (t, o) <- console inst reg
    B8.putStrLn o
    putStrLn $ concat ["last output: ", show t]

vpcs = simpleAws EC2.DescribeVpcs

createVpc :: Text -> RegionAws
createVpc block = simpleAws (EC2.CreateVpc block EC2.Default)

subnets = simpleAws EC2.DescribeSubnets

createSubnet :: Text -> Text -> RegionAws
createSubnet vpc block = simpleAws (EC2.CreateSubnet vpc block)

volumes = simpleAws EC2.DescribeVolumes

volumeStatus = simpleAws EC2.DescribeVolumeStatus

azs = simpleAws EC2.DescribeAvailabilityZones

-- for example: Upcast.Aws.images ["ami-f8d98faa"] "ap-southeast-1" >>= pprint
images :: [Text] -> RegionAws
images amis = simpleAws $ EC2.DescribeImages amis

tags = simpleAws EC2.DescribeTags

keypairs = simpleAws EC2.DescribeKeyPairs

regions = simpleAws EC2.DescribeRegions

knownRegions :: [B.ByteString]
knownRegions = [ "ap-northeast-1"
               , "ap-southeast-1"
               , "ap-southeast-2"
               , "eu-west-1"
               , "eu-central-1" -- coming soon!
               , "sa-east-1"
               , "us-east-1"
               , "us-west-1"
               , "us-west-2"
               ]

status = mapConcurrently ((flip catchAny $ \e -> return $ Left e) . fmap Right . azs) knownRegions
pstatus = fmap rights status >>= mapM_ (LBS.putStrLn . encode)
