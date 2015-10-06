{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Upcast.Infra.AmazonkaTypes where

import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.Trans.AWS (AWST, HasEnv, AWSRequest, AWSPager, send, await)
import qualified Control.Monad.Trans.AWS as AWS
import           Control.Monad.Trans.Resource

import           Data.Monoid (mconcat)
import           Data.Text (Text, unpack)
import           Network.AWS.Types
import           Upcast.Infra.NixTypes (Infras(..))

type ResourceId = Text

type AWSC m = ( MonadCatch m
              , MonadThrow m
              , MonadResource m
              , MonadBaseControl IO m
              , MonadReader AWS.Env m
              )

readRegion s =
  case unpack s of
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

validateRegion :: Infras -> Region
validateRegion Infras{infraRegions} =
    case infraRegions of
      [reg] -> readRegion reg
      _ -> error $ mconcat [ "can only operate with expressions that "
                           , "do not span multiple EC2 regions, given: "
                           , show infraRegions
                           ]
