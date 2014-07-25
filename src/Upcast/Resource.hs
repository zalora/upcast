{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , ScopedTypeVariables
           , DeriveFunctor
           , ExistentialQuantification
           , FlexibleContexts
           , TypeFamilies
           , LambdaCase
           #-}

module Upcast.Resource where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Resource (ResourceT, liftResourceT, runResourceT)
import Control.Monad.Free
import qualified Control.Exception.Lifted as E
import Control.Concurrent (threadDelay)

import Data.Maybe (listToMaybe)
import Data.Monoid
import qualified Data.List as L
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Aeson
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson.Types as A
import qualified Data.Vector as V
import Data.Time.Clock
import Data.IORef (newIORef)
import System.FilePath.Posix (splitFileName)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as Base64

import System.IO (stderr)
import qualified Network.HTTP.Conduit as HTTP
import qualified Aws
import Aws.Core
import Aws.Query (QueryAPIConfiguration(..), castValue)
import qualified Aws.Ec2 as EC2

import Upcast.Types
import Upcast.Command (fgconsume, Command(..), Local(..))

import Upcast.TermSubstitution

import Upcast.Resource.Types
import Upcast.Resource.Ec2

-- | ReaderT context ResourcePlan evaluates in.
data EvalContext = EvalContext
               { mgr :: HTTP.Manager
               , qapi :: QueryAPIConfiguration NormalQuery
               }

txBody :: (ServiceConfiguration r ~ QueryAPIConfiguration, Transaction r Value) =>
          r
          -> EvalContext
          -> ResourceT IO BS.ByteString
txBody tx EvalContext{..} = do
    sig <- liftIO $ canonicalSigData
    let s = signQuery tx qapi sig
    let Just (HTTP.RequestBodyBS key) = sqBody s
    return key
  where
    canonicalSigData :: IO SignatureData
    canonicalSigData = do
        emptyRef <- newIORef []
        return SignatureData { signatureTimeInfo = AbsoluteTimestamp baseTime
                             , signatureTime = baseTime
                             , signatureCredentials = Credentials "" "" emptyRef
                             }

    baseTime = UTCTime (toEnum 0) $ secondsToDiffTime 0


substituteTX :: SubStore -> TX -> EvalContext -> ResourceT IO (Sub, SubStore, Value)
substituteTX state (TX tx) ctx@EvalContext{..} = do
    key <- txBody tx ctx
    -- liftIO $ BS.putStrLn key
    awsConf <- liftIO $ Aws.baseConfiguration
    liftIO $ substitute state (T.decodeUtf8 key) (runResourceT $ Aws.pureAws awsConf qapi mgr tx)


evalPlan :: SubStore -> ResourcePlan a -> ReaderT EvalContext (ResourceT IO) a
evalPlan state (Free (AWSR (TXR tx keyPath) next)) = do
    sub@(t, state', val) <- ask >>= liftResourceT . substituteTX state tx
    let result = acast keyPath val :: Text
    -- liftIO $ print (t, val, result)
    evalPlan state' $ next result
evalPlan state (Free (AWS tx next)) = do
    sub@(t, state', val) <- ask >>= liftResourceT . substituteTX state tx
    -- liftIO $ print (t, val)
    evalPlan state' next
evalPlan state (Free (AWSV (TX tx) next)) = do
    EvalContext{..} <- ask
    awsConf <- liftIO $ Aws.baseConfiguration
    result <- liftResourceT $ Aws.pureAws awsConf qapi mgr tx
    evalPlan state $ next result
evalPlan state (Free (Wait (TX tx) next)) = do
    EvalContext{..} <- ask
    awsConf <- liftIO $ Aws.baseConfiguration
    r <- liftIO $ runResourceT $ retryAws awsConf qapi mgr tx
    -- liftIO $ print r
    evalPlan state next
evalPlan state (Pure r) = return r


txprint :: (ServiceConfiguration r ~ QueryAPIConfiguration, Transaction r Value) =>
           r
           -> ReaderT EvalContext IO ()
txprint tx =
    ask >>= liftIO . runResourceT . txBody tx >>= liftIO . BS.putStrLn

debugPlan :: SubStore -> ResourcePlan a -> ReaderT EvalContext IO a
debugPlan state (Free (AWSR (TXR (TX tx) keyPath) next)) = do
    txprint tx
    debugPlan state $ next "dbg-00000"
debugPlan state (Free (AWS (TX tx) next)) = do
    txprint tx
    debugPlan state next
debugPlan state (Free (AWSV (TX tx) next)) = do
    txprint tx
    debugPlan state $ next $ Array V.empty
debugPlan state (Free (Wait (TX tx) next)) = do
    liftIO (putStrLn "-- wait")
    debugPlan state next
debugPlan state (Pure r) = return r


retryAws :: (ServiceConfiguration r ~ QueryAPIConfiguration, Transaction r Value) =>
            Aws.Configuration
            -> QueryAPIConfiguration NormalQuery
            -> HTTP.Manager
            -> r
            -> ResourceT IO Value
retryAws awsConf conf mgr tx = loop
  where
    loop = do
      result <- catchAll $ Aws.pureAws awsConf conf mgr tx
      case result of
        Left x -> warn x >> loop
        Right Null -> warn Null >> loop
        Right y -> return y

    warn val = liftIO $ do
      BS.hPutStrLn stderr (mconcat ["wait: retrying after 1: ", BS.pack $ show val])
      threadDelay 1000000

    catchAll :: ResourceT IO Value -> ResourceT IO (Either E.SomeException Value)
    catchAll = E.handle (return . Left) . fmap Right


findRegions :: [Text] -> Value -> [Text]
findRegions acc (Object h) = mappend nacc $ join (findRegions [] <$> (fmap snd $ H.toList h))
  where
    nacc = maybe [] (\case String x -> [x]; _ -> []) $ H.lookup "region" h
findRegions acc (Array v) = mappend acc $ join (findRegions [] <$> V.toList v)
findRegions acc _ = acc

debugEvalResources :: DeployContext -> Value -> IO [Machine]
debugEvalResources ctx@DeployContext{..} info = do
    let region = "us-east-1"
    let (keypair, keypairs) = (Nothing, [])
    instances <- do
      let context = EvalContext undefined (QueryAPIConfiguration $ T.encodeUtf8 region)
          action = debugPlan emptyStore (ec2plan name (snd <$> keypairs) info)
          in runReaderT action context

    -- mapM_ LBS.putStrLn $ fmap A.encodePretty instances

    return $ fmap (toMachine keypair) instances
  where
    name = T.pack $ snd $ splitFileName $ T.unpack expressionFile

    toMachine k (h, info) = Machine h
                                    (cast "instancesSet.ipAddress" :: Text)
                                    (cast "instancesSet.privateIpAddress" :: Text)
                                    (cast "instancesSet.instanceId" :: Text)
                                    k
      where
        cast :: FromJSON a => Text -> a
        cast = (`acast` info)

evalResources :: DeployContext -> Value -> IO [Machine]
evalResources ctx@DeployContext{..} info = do
    region <- let regions = L.nub $ findRegions [] info
                  in case regions of
                       reg:[] -> return reg
                       _ -> error $ mconcat [ "can only operate with expressions that "
                                            , "do not span multiple EC2 regions, given: "
                                            , show regions
                                            ]
    store <- loadSubStore stateFile

    -- pre-calculate EC2.ImportKeyPair values here because we need to do IO
    keypairs <- fmap mconcat $ forM (mcast "resources.ec2KeyPairs" info :: [Value]) $ \keypair -> do
                    let (kName, kPK) = parse keypair $ \(Object obj) -> do
                                          kName <- obj .: "name" :: A.Parser Text
                                          kPK <- obj .: "privateKeyFile" :: A.Parser Text
                                          return $ (kName, kPK)
                    pubkey <- fgconsume $ Cmd Local $ mconcat ["ssh-keygen -f ", T.unpack kPK, " -y"]
                    return [(kPK, EC2.ImportKeyPair kName $ T.decodeUtf8 $ Base64.encode pubkey)]

    let keypair = fst <$> listToMaybe keypairs

    instances <- HTTP.withManager $ \mgr -> do
      let context = EvalContext mgr (QueryAPIConfiguration $ T.encodeUtf8 region)
          action = evalPlan store (ec2plan name (snd <$> keypairs) info)
          in runReaderT action context

    -- mapM_ LBS.putStrLn $ fmap A.encodePretty instances

    return $ fmap (toMachine keypair) instances
  where
    name = T.pack $ snd $ splitFileName $ T.unpack expressionFile

    toMachine k (h, info) = Machine h
                                    (cast "instancesSet.ipAddress" :: Text)
                                    (cast "instancesSet.privateIpAddress" :: Text)
                                    (cast "instancesSet.instanceId" :: Text)
                                    k
      where
        cast :: FromJSON a => Text -> a
        cast = (`acast` info)
