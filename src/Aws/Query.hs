-- | AWS Query API

{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           #-}

module Aws.Query (
  module Aws.Query.Types
, QueryData(..)
, querySignQuery
, qArg
, qShow
, valueConsumer
, (+++)
, optional
, optionalA
, enumerate
, enumerateLists
) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Data.Monoid

import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTP

import qualified Text.XML.Cursor as Cu
import Text.XML.Cursor (($//), ($.//))

import Crypto.Hash (hash, Digest, SHA256)
import Data.Byteable (toBytes)

import Aws.Core (SignatureData, SignedQuery(..), Method(..),
                 Protocol(..), signatureTime, fmtTime,
                 authorizationV4, AuthorizationHash(..))
import Aws.Query.Types

data QueryData = QueryData
               { qdEndpoint :: B.ByteString
               , qdRegion :: B.ByteString
               , qdService :: B.ByteString
               }

querySignQuery :: HTTP.Query -> QueryData -> SignatureData -> SignedQuery
querySignQuery query QueryData{..} sd
    = SignedQuery {
        sqMethod = Post
      , sqProtocol = HTTPS
      , sqHost = host
      , sqPort = port
      , sqPath = "/"
      , sqQuery = []
      , sqDate = Just $ signatureTime sd
      , sqAuthorization = Just auth
      , sqContentType = Just contentType
      , sqContentMd5 = Nothing
      , sqAmzHeaders = [("X-Amz-Date", sigTime)]
      , sqOtherHeaders = []
      , sqBody = Just $ HTTP.RequestBodyBS body
      , sqStringToSign = canonicalRequest
      }
    where
        region = qdRegion
        host = qdEndpoint
        port = 443
        sigTime = fmtTime "%Y%m%dT%H%M%SZ" $ signatureTime sd

        body = HTTP.renderQuery False query
        contentType = "application/x-www-form-urlencoded"

        bodyHash = Base16.encode $ toBytes (hash body :: Digest SHA256)

        enumHeaders = "content-type;host;x-amz-date"
        canonicalRequest = B.concat [ "POST\n"
                                    , "/\n"
                                    , "\n" -- query string
                                    , "content-type:"
                                    , contentType
                                    , "\n"
                                    , "host:"
                                    , host
                                    , "\n"
                                    , "x-amz-date:"
                                    , sigTime
                                    , "\n"
                                    , "\n" -- end headers
                                    , enumHeaders
                                    , "\n"
                                    , bodyHash
                                    ]

        auth = authorizationV4 sd HmacSHA256 region qdService
                               enumHeaders
                               canonicalRequest

qArg :: Text -> Maybe B.ByteString
qArg = Just . encodeUtf8

qShow :: Show a => a -> Maybe B.ByteString
qShow = Just . B8.pack . show

-- valueConsumer :: Text -> (Value -> a) -> Cu.Cursor -> Response EC2Metadata a
valueConsumer tag cons cu = go $ head cu'
  where
    cu' = cu $.// Cu.laxElement tag
    -- unwrap = fromJust . H.lookup tag . (\(Object o) -> o)
    unwrap = id
    go = return . cons . unwrap . toValue . Cu.node 

(+++) :: (Monoid a) => a -> a -> a
(+++) = mappend

optional :: ByteString -> Maybe a -> (a -> Maybe ByteString) -> HTTP.Query
optional k (Just x) f = [(k, f x)]
optional k Nothing f = []

optionalA k v = optional k v qArg

enumerate :: String -> [a] -> (a -> Maybe ByteString) -> HTTP.Query
enumerate k xs f = [(B8.pack $ mconcat [k, ".", show n], f x) | (n, x) <- zip ([1..] :: [Int]) xs]

enumerateLists :: ByteString -> [HTTP.Query] -> HTTP.Query
enumerateLists key xs = mconcat [prefix pairs $ mconcat [key, B8.pack $ show n, "."] | (n, pairs) <- zip ([1..] :: [Int]) xs]
  where
    prefix xs p = [(mconcat [p, k], v) | (k, v) <- xs]
