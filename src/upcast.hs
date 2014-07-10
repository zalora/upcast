{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards #-}

module Main where

import System.Environment (getArgs)
import System.FilePath.Posix ((</>))
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Default

import Upcast.Interpolate (n)

import Upcast.State
import Upcast.Nix
import Upcast.PhysicalSpec
import Upcast.Command
import Upcast.Temp
import Upcast.Aws
import Upcast.ATerm (alookupS)

import Upcast.Deploy
import Upcast.Resource
import Upcast.DeployCommands

state DeployContext{..} = do
  (deployment, res) <- runState stateFile $ do
    d <- onlyDeployment deploymentName >>= deploymentAttrs
    rs <- resources d >>= sequence . fmap resourceAttrs
    return (d, rs)

  let Right exprs = fromNix $ fromJust $ lookup "nixExprs" $ deploymentAList deployment
  let machines = filter (\Resource{..} -> resourceType == "ec2") res

  return $ State deployment res exprs machines

buildMachines ctx@DeployContext{..} (State deployment _ exprs machines) = do
  physical <- physicalSpecFile machines
  return $ nixBuildMachines ctx (physical:exprs) (deploymentUuid deployment) (map (T.unpack . resourceName) machines) closuresPath

install DeployContext{..} (State _ _ _ machines) =
    let args m@Resource{..} = (remote m, closuresPath </> (T.unpack resourceName))
        remote m = Remote "/dev/null" (T.unpack $ attr m "publicIpv4")
    in concat [ fmap (uncurry (nixCopyClosureTo sshAuthSock) . args) machines
              , fmap (ssh' sshAuthSock . uncurry nixSetProfile . args) machines
              , fmap (ssh' sshAuthSock . nixSwitchToConfiguration . fst . args) machines
              ]


deployPlan ctx@DeployContext{..} s = do
    Right info <- deploymentInfo ctx s
    print $ fromJust $ alookupS "machines.db1.ec2.subnet" info
    print $ fromJust $ alookupS "resources.subnets" info
    build <- buildMachines ctx s
    let inst = install ctx s
    return $ build:inst

planOnly = do
    let ctx = def :: DeployContext
    s@(State _ resources _ _) <- state ctx
    deployPlan ctx s >>= mapM_ print

infoOnly exprFile = do
    let ctx = def :: DeployContext
    let s = emptyState exprFile
    Right info <- deploymentInfo ctx s
    pprint info

deploy = do
    let ctx = def :: DeployContext
    s@(State _ resources _ _) <- state ctx

    let keypairs = (\Resource{..} -> resourceType == "ec2-keypair") `filter` resources
    print keypairs
    agentSocket <- randomTempFileName "ssh-agent.sock."
    spawn $ sshAgent agentSocket
    mapM_ (fgrun . sshAddKey agentSocket) $ map (flip attr "privateKey") keypairs
    fgrun $ sshListKeys agentSocket

    let ctx' = ctx{ sshAuthSock = T.pack agentSocket }
    deployPlan ctx' s >>= mapM_ fgrun

main = fmap head getArgs >>= infoOnly
