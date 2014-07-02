{-# LANGUAGE DeriveGeneric
           , LambdaCase
           , RecordWildCards
           , NamedFieldPuns
           , OverloadedStrings
           #-}

module Aws.Ec2.Types (
  Value(..)
, toValue
, castValue
) where

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Vector as V
import qualified Data.List as L

import Text.XML (Element(..), Name(..), Node(..))

castValue :: FromJSON a => Value -> Maybe a
castValue v = parseMaybe (const (parseJSON v)) v

toValue :: Node -> Value
toValue = value
  where
    value (NodeElement e@Element{..}) = arrayOrObject e
    value (NodeContent c) = String c
    value _ = Null

    arrayOrValue (x:[]) = x
    arrayOrValue [] = Null
    arrayOrValue a = Array $ V.fromList a

    arrayOrObject Element{ elementName = n
                         , elementNodes = nodes'
                         } = object $ [(name, mapping)]
        where
          mapping = if isXMLArray
                      then Array $ V.fromList $ head $ fmap (fmap value . filterNodes . elementNodes . unElement) nodes
                      else arrayOrValue $ fmap value $ filterNodes nodes'

          name = nameLocalName n
          nodes = onlyElements nodes'
          unElement (NodeElement e) = e

          isXMLArray = ["item"] == (L.nub $ fmap (nameLocalName . elementName . unElement) nodes)

    onlyElements = filter $ \case
                            NodeElement _ -> True
                            _ -> False

    filterNodes = filter $ \case
                              NodeContent s -> (T.strip s) /= T.empty
                              _ -> True
 
