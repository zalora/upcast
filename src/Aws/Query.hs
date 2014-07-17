-- | AWS Query API.
--   See http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-query-api.html

{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , DeriveDataTypeable
           #-}

module Aws.Query (
  module Aws.Query.Types
, QueryData(..)
, QueryMetadata(..)
, QueryError(..)
, querySignQuery
, qArg
, qShow
, valueConsumer
, queryResponseConsumer
, (+++)
, optional
, optionalA
, enumerate
, enumerateLists
) where

import qualified Control.Exception as C
import Control.Monad.Trans.Resource (throwM)
import Control.Monad (mplus)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Data.Monoid
import Data.Maybe
import Data.IORef
import Data.Typeable (Typeable)

import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTP

import qualified Text.XML.Cursor as Cu
import Text.XML.Cursor (($//), ($.//))

import Crypto.Hash (hash, Digest, SHA256)
import Data.Byteable (toBytes)

import Aws.Core
import Aws.Query.Types

data QueryData = QueryData
               { qdEndpoint :: B.ByteString
               , qdRegion :: B.ByteString
               , qdService :: B.ByteString
               }

data QueryError = QueryError
              { queryStatusCode   :: HTTP.Status
              , queryErrorCode    :: Text
              , queryErrorMessage :: Text
              } deriving (Show, Typeable)

instance C.Exception QueryError

data QueryMetadata = QueryMetadata
                 { requestId :: Maybe Text
                 } deriving (Show)

instance Loggable QueryMetadata where
    toLogText (QueryMetadata r) = "Query: requestId=" <> fromMaybe "<none>" r

instance Monoid QueryMetadata where
    mempty = QueryMetadata Nothing
    (QueryMetadata r1) `mappend` (QueryMetadata r2) = QueryMetadata (r1 `mplus` r2)

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

-- valueConsumer :: Text -> (Value -> a) -> Cu.Cursor -> Response QueryMetadata a
valueConsumer tag cons cu = go $ head cu'
  where
    cu' = cu $.// Cu.laxElement tag
    -- unwrap = fromJust . H.lookup tag . (\(Object o) -> o)
    unwrap = id
    go = return . cons . unwrap . toValue . Cu.node 

-- similar: iamResponseConsumer
queryResponseConsumer :: (Cu.Cursor -> Response QueryMetadata a)
                    -> IORef QueryMetadata
                    -> HTTPResponseConsumer a
queryResponseConsumer inner md resp = xmlCursorConsumer parse md resp
  where
    parse cursor = do
      let rid = listToMaybe $ cursor $// elContent "RequestID"
      tellMetadata $ QueryMetadata rid
      case cursor $// Cu.laxElement "Error" of
          []      -> inner cursor
          (err:_) -> fromError err
    fromError cursor = do
      errCode <- force "Missing Error Code"    $ cursor $// elContent "Code"
      errMsg  <- force "Missing Error Message" $ cursor $// elContent "Message"
      throwM $ QueryError (HTTP.responseStatus resp) errCode errMsg

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
