{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Upcast.Infra.Types.Amazonka
( ResourceId
, validateRegion
) where

import Data.Monoid (mconcat)
import Data.Text (Text, unpack)
import Network.AWS.Types (Region(..))

type ResourceId = Text

readRegion :: Text -> Region
readRegion s = case unpack s of
  "eu-west-1"          -> Ireland
  "eu-central-1"       -> Frankfurt
  "ap-northeast-1"     -> Tokyo
  "ap-southeast-1"     -> Singapore
  "ap-southeast-2"     -> Sydney
  "cn-north-1"         -> Beijing
  "us-east-1"          -> NorthVirginia
  "us-west-2"          -> Oregon
  "us-west-1"          -> NorthCalifornia
  "us-gov-west-1"      -> GovCloud
  "fips-us-gov-west-1" -> GovCloudFIPS
  "sa-east-1"          -> SaoPaulo

validateRegion :: [Text] -> Region
validateRegion [region] = readRegion region
validateRegion many = error $ mconcat
  [ "can only operate with expressions that "
  , "do not span multiple EC2 regions, given: "
  , show many
  ]
