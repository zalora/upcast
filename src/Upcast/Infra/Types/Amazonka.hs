module Upcast.Infra.Types.Amazonka
( ResourceId
, validateRegion
, true
) where

import Control.Lens
import Data.Text (Text, unpack)
import Network.AWS.Types (Region(..))

import qualified Network.AWS.EC2 as EC2

type ResourceId = Text

-- "interface EC2APIAttributeServiceValuesFactoryFactory" -- @vlad
true :: Maybe EC2.AttributeBooleanValue
true = Just (EC2.attributeBooleanValue & EC2.abvValue ?~ True)

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
  unk                  -> error ("unknown region " ++ unk)

validateRegion :: [Text] -> Region
validateRegion [region] = readRegion region
validateRegion many = error $ mconcat
  [ "can only operate with expressions that "
  , "do not span multiple EC2 regions, given: "
  , show many
  ]
