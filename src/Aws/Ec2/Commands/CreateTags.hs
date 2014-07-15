{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.Ec2.Commands.CreateTags where

import Aws.Ec2.TH
import qualified Network.HTTP.Types as HTTP

type Tag = (Text, Text)

data CreateTags = CreateTags
               { ct_resources :: [Text]
               , ct_tags :: [Tag]
               } deriving (Show)

enumerateTags :: [Tag] -> HTTP.Query
enumerateTags = enumerateLists "Tag." . fmap unroll
  where
    unroll (key, value) = [ ("Key", qArg key)
                          , ("Value", qArg value)
                          ]

instance SignQuery CreateTags where
    type ServiceConfiguration CreateTags = EC2Configuration
    signQuery CreateTags{..} = ec2SignQuery $
                                           [ ("Action", qArg "CreateTags")
                                           , defVersion
                                           ] +++ enumerate "ResourceId" ct_resources qArg
                                             +++ enumerateTags ct_tags

ec2ValueTransaction ''CreateTags "return"
