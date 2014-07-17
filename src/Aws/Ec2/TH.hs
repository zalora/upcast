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
, module Aws.Query
, module Aws.Ec2.Types
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
import Aws.Query
import Aws.Ec2.Core
import qualified Aws.Ec2.Core as EC2
import Aws.Ec2.Types

ec2ValueTransactionDef :: Name -> Name -> String -> String -> DecsQ
ec2ValueTransactionDef ty cons tag filterKey = do
                arg <- newName "arg"
                [d|
                  instance SignQuery $(conT ty) where
                      type ServiceConfiguration $(conT ty) = EC2Configuration
                      signQuery ($(conP cons [varP arg])) = ec2SignQuery $ [ ("Action", qArg $(stringE $ nameBase ty))
                                                                           , ("Version", qArg "2014-06-15")
                                                                           ] +++ enumerate $(stringE filterKey) $(varE arg) qArg

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
