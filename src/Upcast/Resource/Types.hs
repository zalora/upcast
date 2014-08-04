{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , ScopedTypeVariables
           , DeriveFunctor
           , ExistentialQuantification
           , FlexibleContexts
           , TypeFamilies
           #-}

module Upcast.Resource.Types where
  
import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Free

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.HashMap.Strict as H

import Aws.Core
import Aws.Query
import qualified Aws.Route53 as R53
import qualified Aws.Ec2 as EC2
import qualified Aws.Elb as ELB

import Upcast.Types
import Upcast.ATerm (alookupS, alookupSE)

type MapCast a = Value -> Map Text a

justCast :: FromJSON a => Value -> a
justCast = fromJust . castValue

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

-- | Existential type to contain possible QueryAPI-related transactions for `resourceAWS'
data TX = forall r. (ServiceConfiguration r ~ QueryAPIConfiguration, Transaction r Value) => TX r

-- | Transaction that needs a result, obtained from a Value by Text-typed keypath
data TXR = TXR TX Text

data ResourceF next = AWS TX next
                    | AWSR TXR (Text -> next)
                    | AWSV TX (Value -> next)
                    | Wait TX next
                    | AWS53CRR R53.ChangeResourceRecordSets (Text -> next)
                    deriving (Functor)
type ResourcePlan = Free ResourceF

aws_ :: (MonadFree ResourceF m, ServiceConfiguration r ~ QueryAPIConfiguration, Transaction r Value) => r -> m ()
aws_ tx = liftF (AWS (TX tx) ())

aws1 :: (MonadFree ResourceF m, ServiceConfiguration r ~ QueryAPIConfiguration, Transaction r Value) => r -> Text -> m Text
aws1 tx k = liftF (AWSR (TXR (TX tx) k) id)

aws :: (MonadFree ResourceF m, ServiceConfiguration r ~ QueryAPIConfiguration, Transaction r Value) => r -> m Value
aws tx = liftF (AWSV (TX tx) id)

wait :: (MonadFree ResourceF m, ServiceConfiguration r ~ QueryAPIConfiguration, Transaction r Value) => r -> m ()
wait tx = liftF (Wait (TX tx) ())

aws53crr :: MonadFree ResourceF m => R53.ChangeResourceRecordSets -> m Text
aws53crr crr = liftF (AWS53CRR crr id)

type InstanceA = [(Text, (Text, [(Text, Value)]))] -- (name (id, blockdevices))

parse obj action = either error id (flip parseEither obj action)

--
-- orphanarium:
--

instance FromJSON EC2.IpProtocol where
    parseJSON (String "tcp") = pure EC2.TCP
    parseJSON (String "udp") = pure EC2.UDP
    parseJSON (String "icmp") = pure EC2.ICMP
    parseJSON (String "all") = pure EC2.All
    parseJSON (Number n) = pure $ EC2.Proto 99
    parseJSON Null = pure EC2.All
    parseJSON _ = mzero

instance FromJSON EC2.IpPermission where
    parseJSON (Object v) = EC2.IpPermission <$> v .: "protocol"
                                            <*> v .:? "fromPort"
                                            <*> v .:? "toPort"
                                            <*> (fmap (\a -> [a]) $ v .: "sourceIp")
    parseJSON _ = mzero
