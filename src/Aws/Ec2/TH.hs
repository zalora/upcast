{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           #-}

-- boilerplate minimization for experimental stuff
-- also a hub for module re-exports for ec2 commands

module Aws.Ec2.TH (
  module Aws.Core
, module Aws.Ec2.Core
, module Aws.Ec2.Types
, module Aws.Ec2.Info
, Text
, UTCTime
, FromJSON
, ec2ValueTransactionDef
, ec2ValueTransaction
) where

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

import Data.Text (Text)
import Data.Aeson.Types (FromJSON(..))
import Data.Time.Clock (UTCTime)

import Aws.Core
import Aws.Ec2.Core
import qualified Aws.Ec2.Core as EC2
import Aws.Ec2.Types
import Aws.Ec2.Info

ec2ValueTransactionDef :: Name -> String -> String -> DecsQ
ec2ValueTransactionDef ty action tag = [d|
                  instance SignQuery $(conT ty) where
                      type ServiceConfiguration $(conT ty) = EC2Configuration
                      signQuery _ = ec2SignQuery [ ("Action", qArg $(stringE action))
                                                 , ("Version", qArg "2014-06-15")]

                  instance ResponseConsumer $(conT ty) Value where
                      type ResponseMetadata Value = EC2Metadata
                      responseConsumer _ = ec2ResponseConsumer $ valueConsumer $(stringE tag) id

                  instance Transaction $(conT ty) Value
                  |]

ec2ValueTransaction :: Name -> String -> DecsQ
ec2ValueTransaction ty tag = [d|
                  instance ResponseConsumer $(conT ty) Value where
                      type ResponseMetadata Value = EC2Metadata
                      responseConsumer _ = ec2ResponseConsumer $ valueConsumer $(stringE tag) id

                  instance Transaction $(conT ty) Value
                  |]
