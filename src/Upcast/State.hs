{-# LANGUAGE OverloadedStrings, FlexibleContexts, QuasiQuotes, NamedFieldPuns #-}

module Upcast.State (
  withStateConn
, runState
-- * query functions
, findDeployments
, onlyDeployment
, allDeployments
, resources
, resourceAttrs
, deploymentAttrs
-- * types
, Resource(..)
, Deployment(..)
-- * forward declarations
, Database
) where

import Control.Monad ((>=>))
import Control.Exception (bracket)

import Control.Monad.Reader

import qualified Data.Text as T
import Data.Text (Text)
import Database.SQLite3 (open, close, Database, prepare, bind, SQLData(..), step, finalize, columns, StepResult(..))

import Upcast.Interpolate (n)

runState :: Text -> ReaderT Database IO c -> IO c
runState stateFile value = withStateConn stateFile $ runReaderT value

withStateConn :: Text -> (Database -> IO c) -> IO c
withStateConn stateFile = bracket (open stateFile) close

withStmt conn sql = bracket (prepare conn sql) finalize

runSQL sql args db =
    withStmt db sql $ \stmt -> do
      bind stmt args
      go stmt []
  where
    go stmt acc = do
      result <- step stmt
      case result of
        Row -> do
          cols <- columns stmt
          go stmt (cols:acc)
        Done -> return acc

runSQLM :: Text -> [SQLData] -> ReaderT Database IO [[SQLData]]
runSQLM sql args = ask >>= liftIO . runSQL sql args

data Deployment = Deployment
                { deploymentUuid :: Text
                , deploymentAList :: [(Text, Text)]
                } deriving (Show)
toDeployment [SQLText t] = Deployment t []

data Resource = Resource
              { resourceId :: Integer
              , resourceName :: Text
              , resourceType :: Text
              , resourceAList :: [(Text, Text)]
              } deriving (Show)

toAlist [SQLText name, SQLText value] = (name, value)

findDeployments ident =
    runSQLM sql [SQLText ident, SQLText ident] >>= return . fmap toDeployment
  where
      sql =
        T.pack [n|
               SELECT uuid FROM Deployments d
               WHERE uuid = ? OR EXISTS (SELECT 1 FROM DeploymentAttrs
                                         WHERE deployment = d.uuid AND name = 'name' AND value = ?)
               |]

onlyDeployment ident = findDeployments ident >>= \[deployment] -> return deployment

allDeployments = runSQLM "select uuid from Deployments" [] >>= return . fmap toDeployment

resources (Deployment uuid _) = runSQLM sql [SQLText uuid] >>= return . fmap toResource
  where
    sql = "SELECT id, name, type FROM Resources WHERE deployment = ?"
    toResource [SQLInteger id, SQLText name, SQLText ty] = Resource (fromIntegral id) name ty []

resourceAttrs res@Resource{resourceId, resourceAList=[]} =
    runSQLM sql [SQLInteger $ fromIntegral resourceId] >>= return . (\l -> res{resourceAList=l}) . fmap toAlist
  where
    sql = "SELECT name, value FROM ResourceAttrs WHERE machine = ?"
resourceAttrs res = return res

deploymentAttrs d@(Deployment uuid []) = runSQLM sql [SQLText uuid] >>= return . (\l -> d{deploymentAList=l}) . fmap toAlist
  where
    sql = "SELECT name, value FROM DeploymentAttrs WHERE deployment = ?"
deploymentAttrs d = return d
