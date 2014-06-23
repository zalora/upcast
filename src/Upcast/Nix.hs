{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, ScopedTypeVariables #-}

module Upcast.Nix (
  listToNix
, fromNix
, nixValue
, Value(..)
) where

import Data.List (intercalate)
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)

import Data.Aeson.Types (parseMaybe, parseJSON, FromJSON)

import Upcast.Interpolate (n)
import Upcast.ATerm

listToNix :: [String] -> String
listToNix xs = [n|[ #{intercalate " " $ map (\n -> "\"" ++ n ++ "\"") xs} ]|]

fromNix :: FromJSON a => Text -> Either String a
fromNix t = case nixValue t of
              Left r -> Left r
              Right r' -> case cast r' :: FromJSON a => Maybe a of
                            Nothing -> Left "could not cast"
                            Just j -> Right j
  where
    cast v = parseMaybe (const (parseJSON v)) v

nixValue :: Text -> Either String Value
nixValue = parse . encodeUtf8
