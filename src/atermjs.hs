module Main where

import Prelude hiding (interact)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Lazy.Char8 (interact)
import Data.Aeson.Encode.Pretty (encodePretty)
import Upcast.ATerm (parse)

main = interact (either error encodePretty . parse . toStrict)
