{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           #-}

-- boilerplate minimization for experimental stuff
-- also a hub for module re-exports for query commands

module Aws.Query.TH (
  module Aws.Core
, module Aws.Query
, Text
, UTCTime
, FromJSON
, queryValueTransactionDef
, queryValueTransaction
) where

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

import Data.Text (Text)
import Data.Aeson.Types (FromJSON(..))
import Data.Time.Clock (UTCTime)

import Aws.Core
import Aws.Query

queryValueTransactionDef :: Name -> Name -> String -> Name -> Name -> Name -> String -> DecsQ
queryValueTransactionDef ty cons tag confTy signF version filterKey = do
                arg <- newName "arg"
                [d|
                  instance SignQuery $(conT ty) where
                      type ServiceConfiguration $(conT ty) = $(conT confTy)
                      signQuery ($(conP cons [varP arg])) = $(varE signF) $ [ ("Action", qArg $(stringE $ nameBase ty))
                                                                            , $(varE version)
                                                                            ] +++ enumerate $(stringE filterKey) $(varE arg) qArg

                  instance ResponseConsumer $(conT ty) Value where
                      type ResponseMetadata Value = QueryMetadata
                      responseConsumer _ = queryResponseConsumer $ valueConsumer $(stringE tag) id

                  instance Transaction $(conT ty) Value
                  |]

queryValueTransaction :: Name -> String -> DecsQ
queryValueTransaction ty tag = [d|
                  instance ResponseConsumer $(conT ty) Value where
                      type ResponseMetadata Value = QueryMetadata
                      responseConsumer _ = queryResponseConsumer $ valueConsumer $(stringE tag) id

                  instance Transaction $(conT ty) Value
                  |]
