module Upcast.Nix (
  jsonText
, nixValue
, nixInfras
) where

import Data.Text (Text)
import Data.ByteString.Char8 (ByteString)

import Data.Aeson (eitherDecodeStrict, Value)
import Upcast.Infra.Nix (Infras)

jsonText :: ByteString -> Either String Text
jsonText = eitherDecodeStrict

nixValue :: ByteString -> Either String Value
nixValue = eitherDecodeStrict

nixInfras :: ByteString -> Either String Infras
nixInfras = eitherDecodeStrict
