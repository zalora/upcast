{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           #-}

module Aws.Elb.TH (
  module Aws.Core
, module Aws.Elb.Core
, module Aws.Query
, Text
, FromJSON
, elbValueTransaction
, elbValueTransactionDef
) where

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

import Data.Text (Text)
import Data.Aeson.Types (FromJSON(..))

import Aws.Core
import Aws.Query
import Aws.Query.TH
import Aws.Elb.Core

elbValueTransaction :: Name -> String -> DecsQ
elbValueTransaction ty tag = [d|
                  instance ResponseConsumer $(conT ty) Value where
                      type ResponseMetadata Value = QueryMetadata
                      responseConsumer _ = queryResponseConsumer $ valueConsumerOpt (XMLValueOptions "member") $(stringE tag) id

                  instance Transaction $(conT ty) Value
                  |]

elbValueTransactionDef ty cons tag filterKey = queryValueTransactionDef ty cons tag 'elbSignQuery 'defVersion "member" filterKey
