{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Upcast.Aws where

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

type DescribeFun = B.ByteString -> IO Value

simpleAws :: (Transaction r Value, Aws.ServiceConfiguration r ~ EC2Configuration) => r -> B.ByteString -> IO Value
simpleAws arg region = do
    -- cfg <- Aws.dbgConfiguration
    cfg <- Aws.baseConfiguration
    Aws.simpleAws cfg (EC2Configuration region) arg

pprint :: ToJSON a => a -> IO ()
pprint = LBS.putStrLn . encodePretty

instances :: DescribeFun
instances = simpleAws EC2.DescribeInstances

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

vpcs :: DescribeFun
vpcs = simpleAws EC2.DescribeVpcs

createVpc :: Text -> DescribeFun
createVpc block = simpleAws (EC2.CreateVpc block EC2.Default)

subnets :: DescribeFun
subnets = simpleAws EC2.DescribeSubnets

createSubnet :: Text -> Text -> DescribeFun
createSubnet vpc block = simpleAws (EC2.CreateSubnet vpc block)

volumes :: DescribeFun
volumes = simpleAws EC2.DescribeVolumes

volumeStatus :: DescribeFun
volumeStatus = simpleAws EC2.DescribeVolumeStatus

azs :: DescribeFun
azs = simpleAws EC2.DescribeAvailabilityZones

images :: DescribeFun
images = simpleAws EC2.DescribeImages

tags :: DescribeFun
tags = simpleAws EC2.DescribeTags

keypairs :: DescribeFun
keypairs = simpleAws EC2.DescribeKeyPairs

regions :: DescribeFun
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
