{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , ScopedTypeVariables
           , DeriveFunctor
           , ExistentialQuantification
           , FlexibleContexts
           , TypeFamilies
           #-}

module Upcast.Infra.Types where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Free

import Data.Monoid (mconcat)

import Data.Text (Text, split)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Aeson
import Data.Aeson.Types (parseEither, Parser)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

import Aws.Core
import Aws.Query
import qualified Aws.Route53 as R53
import qualified Aws.Ec2 as EC2
import qualified Aws.Elb as ELB

import Upcast.Types
import Upcast.Infra.Nix

type MapCast a = Value -> Map Text a

alookup :: [Text] -> Value -> Maybe Value
alookup keys o@(Object h) =
    case keys of
      (x:xs) -> H.lookup x h >>= alookup xs
      [] -> return o
alookup keys@(_:_) a@(Array v) =
    case V.length v of
      1 -> alookup keys $ V.head v
      _ -> Nothing
alookup [] o = return o
alookup (_:_) _ = Nothing

alookupS = alookup . split (== '.')

alookupSE s v = maybe (Left $ mconcat ["key `", s, "' not found"]) Right $ alookup (split (== '.') s) v

castE :: FromJSON a => Value -> Either String a
castE v = parseEither (const (parseJSON v)) v

justCast :: forall a. FromJSON a => Value -> a
justCast v = either (\e -> error (concat ["justCast failed for: ", show v, " with: ", e])) id (castE v :: Either String a)

mvalues = fmap snd . Map.toList
values = fmap snd . H.toList

forceLookup k v = case alookupSE k v of
                    Left reason -> error $ show reason
                    Right x -> x

mcast :: forall a. FromJSON a => Text -> Value -> [a]
mcast key value = mvalues $ (justCast :: MapCast a) $ forceLookup key value

acast :: forall a. FromJSON a => Text -> Value -> a
acast key value = (justCast :: Value -> a) $ forceLookup key value

scast :: forall a. FromJSON a => Text -> Value -> Maybe a
scast k v = alookupS k v >>= castValue

alistFromObject :: Text -> Value -> [(Text, Value)]
alistFromObject key value = Map.toList $ (justCast :: MapCast Value) $ forceLookup key value

castText :: Value -> Maybe Text
castText (String "") = Nothing
castText (String s) = Just s
castText _ = Nothing

type K = Text
type V = Text
type IDAlist = [(K, V)]

lookupOrId' :: Text -> IDAlist -> InfraRef a -> Maybe V
lookupOrId' prefix alist (RefLocal x) = lookupOrId prefix alist x
lookupOrId' prefix alist (RefRemote x) = lookupOrId prefix alist x

lookupOrId :: Text -> IDAlist -> K -> Maybe V
lookupOrId prefix alist str =
  case prefix `T.isPrefixOf` str of
    True -> Just str
    False -> lookup str alist

type Tags = [EC2.Tag]

forAttrs :: Applicative f => Map k v -> (k -> v -> f x) -> f [(k, x)]
forAttrs xs f = Map.toList <$> Map.traverseWithKey f xs


-- | Existential type to contain possible QueryAPI-related transactions for `resourceAWS'
data TX = forall r. (ServiceConfiguration r ~ QueryAPIConfiguration, Transaction r Value) => TX r

-- | Transaction that needs a result, obtained from a Value by Text-typed keypath
data TXR = TXR TX Text

data InfraF next = AWS TX next
                    | AWSR TXR (Text -> next)
                    | AWSV TX (Value -> next)
                    | Log Text next
                    | Wait TX next
                    | AWS53CRR R53.ChangeResourceRecordSets (Text -> next)
                    deriving (Functor)
type InfraPlan = Free InfraF

aws_ :: (MonadFree InfraF m, ServiceConfiguration r ~ QueryAPIConfiguration, Transaction r Value) => r -> m ()
aws_ tx = liftF (AWS (TX tx) ())

aws1 :: (MonadFree InfraF m, ServiceConfiguration r ~ QueryAPIConfiguration, Transaction r Value) => r -> Text -> m Text
aws1 tx k = liftF (AWSR (TXR (TX tx) k) id)

aws :: (MonadFree InfraF m, ServiceConfiguration r ~ QueryAPIConfiguration, Transaction r Value) => r -> m Value
aws tx = liftF (AWSV (TX tx) id)

awslog :: MonadFree InfraF m => Text -> m ()
awslog text = liftF (Log text ())

wait :: (MonadFree InfraF m, ServiceConfiguration r ~ QueryAPIConfiguration, Transaction r Value) => r -> m ()
wait tx = liftF (Wait (TX tx) ())

aws53crr :: MonadFree InfraF m => R53.ChangeResourceRecordSets -> m Text
aws53crr crr = liftF (AWS53CRR crr id)

type InstanceA = [(Text, (Text, [(Text, BlockDeviceMapping)]))] -- | (name (id, blockdevices))
type UserDataA = [(Text, HashMap Text Text)] -- | (machineName, key -> value)

parse :: a -> (a -> Parser b) -> b
parse obj action = either error id (flip parseEither obj action)
