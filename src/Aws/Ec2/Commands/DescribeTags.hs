{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           #-}

module Aws.Ec2.Commands.DescribeTags where

import Aws.Ec2.TH

data DescribeTags = DescribeTags [Text]
                       deriving (Show)

ec2ValueTransactionDef ''DescribeTags 'DescribeTags "tagSet" "" -- search not implemented
