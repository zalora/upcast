{-# LANGUAGE ExistentialQuantification #-}

module Upcast.Infra.Types.Infra
( Infra(..)
) where

import Control.Applicative ((<$>), (<|>))
import Data.Aeson.Types (Value(..), FromJSON(..), Parser, parseMaybe, (.:))
import Upcast.Infra.Types.Resource (Resource(..))
import Upcast.Infra.NixTypes -- (*)

data Infra = forall a. (FromJSON a, Resource a, Show a) => Infra a

instance Resource Infra where
  match (Infra infra) = match infra
  create (Infra infra) = create infra
  update current (Infra infra) = update current infra
  reify ledger (Infra infra) = Infra <$> reify ledger infra

-- XXX: These choices are strictly ordered because some JSON representations
-- are ambiguous with respect to each other absent '_name'. Something like
-- 'o .: _name ~ "ec2-subnet" *> parseJSON value :: ...' might help, but I
-- haven't tried it yet.
instance FromJSON Infra where
  parseJSON value = (Infra <$> (parseJSON value :: Parser Ebs))
                <|> (Infra <$> (parseJSON value :: Parser Ec2instance))
                <|> (Infra <$> (parseJSON value :: Parser Ec2keypair))
                <|> (Infra <$> (parseJSON value :: Parser Ec2subnet))
                <|> (Infra <$> (parseJSON value :: Parser Ec2sg))
                <|> (Infra <$> (parseJSON value :: Parser Ec2sgruleset))
                <|> (Infra <$> (parseJSON value :: Parser Ec2vpc))
                <|> (Infra <$> (parseJSON value :: Parser Elb))

instance Show Infra where
  showsPrec n (Infra a) = showsPrec n a
