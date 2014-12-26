{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , NamedFieldPuns
           , ScopedTypeVariables
           , DeriveFunctor
           , ExistentialQuantification
           , FlexibleContexts
           , TypeFamilies
           , LambdaCase
           #-}

module Upcast.Infra where

import Prelude hiding (sequence)

import Control.Applicative
import Control.Monad.Reader hiding (sequence, forM)
import Control.Monad.Trans.Resource (ResourceT, liftResourceT, runResourceT)
import Control.Monad.Trans.Resource (MonadBaseControl)
import Control.Monad.Free
import qualified Control.Exception.Lifted as E
import Control.Concurrent (threadDelay)
import System.IO

import Data.Maybe (listToMaybe)
import Data.Monoid
import Data.Traversable
import qualified Data.List as L
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Aeson
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson.Types as A
import qualified Data.Vector as V
import System.FilePath.Posix (splitFileName)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString.Lazy (toStrict)

import System.IO (stderr)
import qualified Network.HTTP.Conduit as HTTP
import qualified Aws
import Aws.Core
import Aws.Query (QueryAPIConfiguration(..), castValue)
import Aws.Canonical (canonicalSigData)
import qualified Aws.Ec2 as EC2
import qualified Aws.Route53 as R53

import Upcast.Types
import Upcast.Command (fgconsume_, Command(..), Local(..))

import Upcast.TermSubstitution

import Upcast.Infra.Types
import Upcast.Infra.Ec2

-- | ReaderT context InfraPlan evaluates in.
data EvalContext = EvalContext
               { mgr :: HTTP.Manager
               , awsConf :: Aws.Configuration
               , qapi :: QueryAPIConfiguration NormalQuery
               , route53 :: R53.Route53Configuration NormalQuery
               }

rqBody :: (MonadIO io, SignQuery r) => r -> ServiceConfiguration r q -> io Text
rqBody tx conf = do
    sig <- liftIO $ canonicalSigData
    let s = signQuery tx conf sig
    let Just body = sqBody s
    return $ bodyText body
  where
    bodyText :: HTTP.RequestBody -> Text
    bodyText (HTTP.RequestBodyBS bs) = T.decodeUtf8 bs
    bodyText (HTTP.RequestBodyLBS lbs) = T.decodeUtf8 $ toStrict lbs

substituteTX :: SubStore -> TX -> EvalContext -> ResourceT IO (Sub, SubStore, Value)
substituteTX state (TX tx) EvalContext{..} = do
    key <- rqBody tx qapi
    -- liftIO $ T.putStrLn key
    liftIO $ substitute state key (runResourceT $ Aws.pureAws awsConf qapi mgr tx)

substitute_ :: SubStore -> Text -> IO any -> ResourceT IO (Sub, SubStore, Value)
substitute_ state key action = liftIO $ do
    -- T.hPutStrLn stderr key
    substitute state key (action >> return Null)

evalPlan :: SubStore -> InfraPlan a -> ReaderT EvalContext (ResourceT IO) a
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
    result <- liftResourceT $ Aws.pureAws awsConf qapi mgr tx
    -- liftIO $ print result
    evalPlan state $ next result
evalPlan state (Free (Wait (TX tx) next)) = do
    EvalContext{..} <- ask
    desc <- txshow tx
    r <- liftIO $ runResourceT $ retry desc (Aws.pureAws awsConf qapi mgr tx) awsTest
    -- liftIO $ print r
    evalPlan state next
evalPlan state (Free (Log t next)) = do
    liftIO . T.hPutStrLn stderr $ t
    evalPlan state next
evalPlan state (Free (AWS53CRR crr next)) = do
    EvalContext{..} <- ask
    txb <- liftIO $ rqBody crr route53
    (_, state', val) <- liftResourceT $ substitute_ state txb (runResourceT $
                      retry "ChangeResourceRecordSets" (Aws.pureAws awsConf route53 mgr crr) r53Test)
    evalPlan state' $ next "ok"
evalPlan state (Pure r) = return r


txshow :: (MonadIO io, ServiceConfiguration r ~ QueryAPIConfiguration, Transaction r Value)
          => r
          -> ReaderT EvalContext io Text
txshow tx = do
    EvalContext{qapi} <- ask
    rqBody tx qapi

debugPlan :: SubStore -> InfraPlan a -> ReaderT EvalContext IO a
debugPlan state (Free (AWSR (TXR (TX tx) keyPath) next)) = do
    txshow tx >>= liftIO . T.putStrLn
    debugPlan state $ next "dbg-00000"
debugPlan state (Free (AWS (TX tx) next)) = do
    txshow tx >>= liftIO . T.putStrLn
    debugPlan state next
debugPlan state (Free (AWSV (TX tx) next)) = do
    txshow tx >>= liftIO . T.putStrLn
    debugPlan state $ next $ Array V.empty
debugPlan state (Free (Wait (TX tx) next)) = do
    liftIO (putStrLn "-- wait")
    debugPlan state next
debugPlan state (Free (Log t next)) = do
    liftIO . T.hPutStrLn stderr $ t
    debugPlan state next
debugPlan state (Free (AWS53CRR crr next)) = do
    EvalContext{..} <- ask
    txb <- liftIO $ rqBody crr route53
    liftIO $ print (crr, txb)
    debugPlan state $ next "dbg-ok"
debugPlan state (Pure r) = return r


retry :: forall exc ret m. (E.Exception exc, MonadIO m, MonadBaseControl IO m)
         => Text
         -> m ret
         -> (Either exc ret -> Either String ret)
         -> m ret
retry desc action test = loop Nothing
  where
    loop lastErr = do
        result <- catchAll action
        case (test result) of
          Left reason -> warn reason >> loop (Just reason)
          Right v -> do
            when (lastErr == Nothing) $ liftIO $ T.hPutStrLn stderr ""
            return v

      where
        warn val = liftIO $ do
          case maybe False (== val) lastErr of
            False -> T.hPutStrLn stderr (T.concat ["retrying <", desc, "> after 1: ", T.pack val])
            True -> T.hPutStr stderr "." >> hFlush stderr
          threadDelay 1000000

    catchAll :: m ret -> m (Either exc ret)
    catchAll = E.handle (return . Left) . fmap Right

awsTest :: Either E.SomeException Value -> Either String Value
awsTest (Left x) = Left $ show x
awsTest (Right Null) = Left $ show Null
awsTest (Right r) = Right r

r53Test :: Either R53.Route53Error a -> Either String a
r53Test (Left x) = Left $ show x
r53Test (Right r) = Right r


findRegions :: [Text] -> Value -> [Text]
findRegions acc (Object h) = mappend nacc $ join (findRegions [] <$> (fmap snd $ H.toList h))
  where
    nacc = maybe [] (\case String x -> [x]; _ -> []) $ H.lookup "region" h
findRegions acc (Array v) = mappend acc $ join (findRegions [] <$> V.toList v)
findRegions acc _ = acc

-- | read files mentioned in userData for each instance
preReadUserData :: Value -> IO UserDataA
preReadUserData info =
    fmap mconcat $ forM (alistFromObject "ec2-instance" info) $ \inst -> do
        let (name, dataA) = parse inst $ \(name, Object ec2) -> do
              dataA :: HashMap Text Text <- ec2 .: "userData"
              return (name, dataA)
        readA <- sequence $ fmap (T.readFile . T.unpack) dataA
        return [(name, readA)]

-- | pre-calculate EC2.ImportKeyPair values while we can do IO
prepareKeyPairs :: Value -> IO [(Text, EC2.ImportKeyPair)]
prepareKeyPairs info =
    fmap mconcat $ forM (mcast "ec2-keypair" info :: [Value]) $ \keypair -> do
        let (kName, kPK) = parse keypair $ \(Object obj) -> do
                              kName <- obj .: "name" :: A.Parser Text
                              kPK <- obj .: "privateKeyFile" :: A.Parser Text
                              return $ (kName, kPK)
        pubkey <- fgconsume_ $ Cmd Local (mconcat ["ssh-keygen -f ", T.unpack kPK, " -y"]) "ssh-keygen"
        return [(kPK, EC2.ImportKeyPair kName $ T.decodeUtf8 $ Base64.encode pubkey)]

toMachine k (h, reportedInfo, infraInfo) = Machine h
                                (cast "instancesSet.ipAddress" :: Text)
                                (cast "instancesSet.privateIpAddress" :: Text)
                                (cast "instancesSet.instanceId" :: Text)
                                k
                                (acast "nix" infraInfo :: Bool)
  where
    cast :: FromJSON a => Text -> a
    cast = (`acast` reportedInfo)

debugEvalInfra :: InfraContext -> IO [Machine]
debugEvalInfra InfraContext{..} = do
    let region = "us-east-1"
    let (keypair, keypairs) = (Nothing, [])

    userDataA <- preReadUserData inc_data

    instances <- do
      let qapi = QueryAPIConfiguration $ T.encodeUtf8 region
          context = EvalContext undefined undefined qapi R53.route53
          action = debugPlan emptyStore (ec2plan name (snd <$> keypairs) inc_data userDataA)
          in runReaderT action context

    -- mapM_ LBS.putStrLn $ fmap A.encodePretty instances

    return $ fmap (toMachine keypair) instances
  where
    name = T.pack $ snd $ splitFileName $ inc_expressionFile

evalInfra :: InfraContext -> IO [Machine]
evalInfra InfraContext{..} = do
    region <- let regions = L.nub $ findRegions [] inc_data
                  in case regions of
                       reg:[] -> return reg
                       _ -> error $ mconcat [ "can only operate with expressions that "
                                            , "do not span multiple EC2 regions, given: "
                                            , show regions
                                            ]
    store <- loadSubStore inc_stateFile

    keypairs <- prepareKeyPairs inc_data
    let keypair = fst <$> listToMaybe keypairs

    userDataA <- preReadUserData inc_data

    instances <- HTTP.withManager $ \mgr -> do
      awsConf <- liftIO $ Aws.baseConfiguration
      let context = EvalContext mgr awsConf (QueryAPIConfiguration $ T.encodeUtf8 region) R53.route53
          action = evalPlan store (ec2plan name (snd <$> keypairs) inc_data userDataA)
          in runReaderT action context

    -- mapM_ LBS.putStrLn $ fmap A.encodePretty instances

    return $ fmap (toMachine keypair) instances
  where
    name = T.pack $ snd $ splitFileName $ inc_expressionFile
