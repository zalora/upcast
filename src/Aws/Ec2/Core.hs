{-# LANGUAGE OverloadedStrings
           , FlexibleInstances
           , DeriveDataTypeable
           , RecordWildCards
           , TypeFamilies
           #-}

module Aws.Ec2.Core (
  EC2Configuration(..)
, EC2Metadata
, ec2SignQuery
, ec2ResponseConsumer
, valueConsumer
, qArg
, qShow
, defVersion
) where

import qualified Control.Exception as C
import Control.Monad
-- import Control.Monad.Trans.Resource (MonadThrow)
import Control.Monad.Trans.Resource (throwM)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.String (IsString)

import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Data.Byteable
import Data.IORef

import Crypto.Hash

import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTP

import qualified Text.XML.Cursor as Cu
import Text.XML.Cursor (($//), ($.//))

import Aws.Core
import Aws.Ec2.Types

instance AsMemoryResponse Value where
    type MemoryResponse Value = Value
    loadToMemory = return

data EC2Configuration qt = EC2Configuration
                         { ec2Region :: B.ByteString
                         } deriving (Show)

instance DefaultServiceConfiguration (EC2Configuration NormalQuery) where
  defServiceConfig = EC2Configuration "us-east-1"
  debugServiceConfig = EC2Configuration "us-east-1"

data EC2Error = EC2Error
              { ec2StatusCode   :: HTTP.Status
              , ec2ErrorCode    :: Text
              , ec2ErrorMessage :: Text
              } deriving (Show, Typeable)

instance C.Exception EC2Error

data EC2Metadata = EC2Metadata
                 { requestId :: Maybe Text
                 } deriving (Show, Typeable)

instance Loggable EC2Metadata where
    toLogText (EC2Metadata r) = "EC2: requestId=" <> fromMaybe "<none>" r

instance Monoid EC2Metadata where
    mempty = EC2Metadata Nothing
    (EC2Metadata r1) `mappend` (EC2Metadata r2) = EC2Metadata (r1 `mplus` r2)

qArg :: Text -> Maybe B.ByteString
qArg = Just . encodeUtf8

qShow :: Show a => a -> Maybe B.ByteString
qShow = Just . B8.pack . show

defVersion :: HTTP.QueryItem
defVersion = ("Version", Just "2014-06-15")

-- similar: dySignQuery
ec2SignQuery :: HTTP.Query -> EC2Configuration qt -> SignatureData -> SignedQuery
ec2SignQuery query EC2Configuration{..} sd
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
        region = ec2Region
        host = B.concat ["ec2.", region, ".amazonaws.com"]
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

        auth = authorizationV4 sd HmacSHA256 region "ec2"
                               enumHeaders
                               canonicalRequest


-- similar: iamResponseConsumer
ec2ResponseConsumer :: (Cu.Cursor -> Response EC2Metadata a)
                    -> IORef EC2Metadata
                    -> HTTPResponseConsumer a
ec2ResponseConsumer inner md resp = xmlCursorConsumer parse md resp
  where
    parse cursor = do
      let rid = listToMaybe $ cursor $// elContent "RequestID"
      tellMetadata $ EC2Metadata rid
      case cursor $// Cu.laxElement "Error" of
          []      -> inner cursor
          (err:_) -> fromError err
    fromError cursor = do
      errCode <- force "Missing Error Code"    $ cursor $// elContent "Code"
      errMsg  <- force "Missing Error Message" $ cursor $// elContent "Message"
      throwM $ EC2Error (HTTP.responseStatus resp) errCode errMsg

valueConsumer :: Text -> (Value -> a) -> Cu.Cursor -> Response EC2Metadata a
valueConsumer tag cons cu = go $ head cu'
  where
    cu' = cu $.// Cu.laxElement tag
    -- unwrap = fromJust . H.lookup tag . (\(Object o) -> o)
    unwrap = id
    go = return . cons . unwrap . toValue . Cu.node 

