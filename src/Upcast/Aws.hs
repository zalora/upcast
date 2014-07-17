{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Upcast.Aws (
  simpleAws
, pprint
, instances
, instanceStatus
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
, elbs
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
import Aws.Query (QueryAPIConfiguration(..))
import Aws.Ec2

import qualified Aws.Ec2 as EC2
import qualified Aws.Elb as ELB

catchAny :: IO a -> (Control.Exception.SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

type RegionAws = B.ByteString -> IO Value

simpleAws :: (Transaction r Value, Aws.ServiceConfiguration r ~ QueryAPIConfiguration) => r -> RegionAws
simpleAws arg region = do
    -- cfg <- Aws.dbgConfiguration
    cfg <- Aws.baseConfiguration
    Aws.simpleAws cfg (QueryAPIConfiguration region) arg

pprint :: ToJSON a => a -> IO ()
pprint = LBS.putStrLn . encodePretty

instances ids = simpleAws $ EC2.DescribeInstances ids

instanceStatus ids = simpleAws $ EC2.DescribeInstanceStatus ids

securityGroups :: [EC2.SecurityGroupId] -> [EC2.SecurityGroupName] -> RegionAws
securityGroups ids names = simpleAws $ EC2.DescribeSecurityGroups ids names

securityGroupsByName name = securityGroups [] [name]

createSecurityGroup :: Text -> Text -> Text -> RegionAws
createSecurityGroup name desc vpcId = simpleAws $ EC2.CreateSecurityGroup name desc $ Just vpcId

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

vpcs ids = simpleAws $ EC2.DescribeVpcs []

createVpc :: Text -> RegionAws
createVpc block = simpleAws (EC2.CreateVpc block EC2.Default)

subnets ids = simpleAws $ EC2.DescribeSubnets []

createSubnet :: Text -> Text -> Maybe Text -> RegionAws
createSubnet vpc block az = simpleAws (EC2.CreateSubnet vpc block az)

volumes ids = simpleAws $ EC2.DescribeVolumes ids

volumeStatus ids = simpleAws $ EC2.DescribeVolumeStatus ids

azs names = simpleAws $ EC2.DescribeAvailabilityZones names

-- for example: Upcast.Aws.images ["ami-f8d98faa"] "ap-southeast-1" >>= pprint
images :: [Text] -> RegionAws
images amis = simpleAws $ EC2.DescribeImages amis

tags = simpleAws $ EC2.DescribeTags []

keypairs names = simpleAws $ EC2.DescribeKeyPairs names

regions names = simpleAws $ EC2.DescribeRegions names

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

elbs names = simpleAws $ ELB.DescribeLoadBalancers names

status = mapConcurrently ((flip catchAny $ \e -> return $ Left e) . fmap Right . azs []) knownRegions
pstatus = fmap rights status >>= mapM_ (LBS.putStrLn . encode)
