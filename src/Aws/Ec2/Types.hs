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

-- import Debug.Trace (trace)
-- import Text.Show.Pretty (ppShow)
-- 
-- traceArg a = ppShow a `trace` a
traceArg = id

castValue :: FromJSON a => Value -> Maybe a
castValue v = parseMaybe (const (parseJSON v)) v

toValue = value

value :: Node -> Value
value (NodeElement e@Element{..}) = values elementNodes 
value (NodeContent c) = String c
value _ = Null

values elementNodes = uncurry elementValues $ traceArg $ (elementKind elementNodes, elementNodes)

data ElementKind = ObjectLike
                 | ArrayLike
                 | Other
                 deriving (Show)

elementKind nodes = if isXMLArray then ArrayLike
                                  else if isObject then ObjectLike
                                                   else Other
  where
    filtered = filterNodes nodes
    elems = onlyElements nodes

    isObject = (not $ null elems) && length filtered == length elems
    isXMLArray = ["item"] == (L.nub $ fmap forceElementName elems)

elementValues :: ElementKind -> [Node] -> Value
elementValues ObjectLike ns = object [(forceElementName n, values $ filterNodes $ elementNodes $ unElement n) | n <- onlyElements ns]
elementValues ArrayLike ns = array $ innerNodes ns
  where
    innerNodes :: [Node] -> [Value]
    innerNodes nodes = fmap (values . filterNodes . elementNodes . unElement) $ onlyElements $ filterNodes $ nodes
elementValues Other ns = arrayOrValue $ fmap value ns
  where
    arrayOrValue (x:[]) = x
    arrayOrValue [] = Null
    arrayOrValue a = array a

forceElementName = nameLocalName . elementName . unElement
unElement (NodeElement e) = e
array = Array . V.fromList

onlyElements = filter $ \case
                        NodeElement _ -> True
                        _ -> False

filterNodes = filter $ \case
                          NodeContent s -> (T.strip s) /= T.empty
                          _ -> True
