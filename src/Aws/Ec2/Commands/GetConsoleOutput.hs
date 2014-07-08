{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           , RecordWildCards
           , DeriveGeneric
           #-}

module Aws.Ec2.Commands.GetConsoleOutput where

import Aws.Ec2.TH
import GHC.Generics

data GetConsoleOutput = GetConsoleOutput { gco_instanceId :: Text }
                       deriving (Show)

data ConsoleOutput = ConsoleOutput
                   { requestId :: Text
                   , instanceId :: Text
                   , timestamp :: UTCTime
                   , output :: Text
                   } deriving (Generic, Show)

instance FromJSON ConsoleOutput

instance SignQuery GetConsoleOutput where
    type ServiceConfiguration GetConsoleOutput = EC2Configuration
    signQuery GetConsoleOutput{..} = ec2SignQuery [ ("Action", qArg "GetConsoleOutput")
                                                  , defVersion
                                                  , ("InstanceId", qArg gco_instanceId)
                                                  ]

ec2ValueTransaction ''GetConsoleOutput "GetConsoleOutputResponse"
