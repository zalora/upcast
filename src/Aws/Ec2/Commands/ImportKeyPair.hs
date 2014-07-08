{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.Ec2.Commands.ImportKeyPair where

import Data.Text (Text)
import Aws.Ec2.TH

data ImportKeyPair = ImportKeyPair
               { ikp_keyName :: Text
               , ikp_publicKeyMaterial :: Text
               } deriving (Show)

instance SignQuery ImportKeyPair where
    type ServiceConfiguration ImportKeyPair = EC2Configuration
    signQuery ImportKeyPair{..} = ec2SignQuery $ 
                                           [ ("Action", qArg "ImportKeyPair")
                                           , defVersion
                                           , ("KeyName", qArg ikp_keyName)
                                           , ("PublicKeyMaterial", qArg ikp_publicKeyMaterial)
                                           ]

ec2ValueTransaction ''ImportKeyPair "ImportKeyPairResponse"
