-- | NixOps compat stuff, suffers from code duplication due to fast deprecation

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards #-}

module Upcast.Nixops where

import System.FilePath

import Data.Maybe (fromJust)
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T

import Upcast.Interpolate (n)
import Upcast.Nix
import Upcast.State
import Upcast.Command
import Upcast.Deploy (setupAgent)
import Upcast.DeployCommands hiding (nixBaseOptions, nixBuildMachines, nixDeploymentInfo)
import Upcast.PhysicalSpec
import Upcast.ATerm (alookupS)

data DeployContext =
    DeployContext { nixops, nixPath, stateFile, deploymentName, key, sshAuthSock :: Text
                  , closuresPath :: String
                  } deriving (Show)

-- sorry guys, you'll have to impersonate me for now
instance Default DeployContext where
    def = DeployContext
          { nixops = "/tank/proger/dev/upcast/nix"
          , nixPath = "sources"
          , stateFile = "deployments.nixops"
          , deploymentName = "staging"
          , key = "id_rsa.tmp"
          , sshAuthSock = "/dev/null"
          , closuresPath = "/tmp/machines/1"
          }


attr Resource{..} = fromJust . flip lookup resourceAList
dattr Deployment{..} = fromJust . flip lookup deploymentAList

state DeployContext{..} = do
  (deployment, res) <- runState stateFile $ do
    d <- onlyDeployment deploymentName >>= deploymentAttrs
    rs <- resources d >>= sequence . fmap resourceAttrs
    return (d, rs)

  let Right exprs = fromNix $ fromJust $ lookup "nixExprs" $ deploymentAList deployment
  let machines = filter (\Resource{..} -> resourceType == "ec2") res

  return $ State deployment res exprs machines

nixBaseOptions DeployContext{..} = [n|
                 -I #{nixPath}
                 -I nixops=#{nixops}
                 --option use-binary-cache true
                 --option binary-caches http://hydra.nixos.org
                 --option use-ssh-substituter true
                 --option ssh-substituter-hosts me@node1.example.com
                 --show-trace
                 |]


nixDeploymentInfo ctx exprs uuid = Cmd Local [n|
                     nix-instantiate #{nixBaseOptions ctx}
                     --arg networkExprs '#{listToNix exprs}'
                     --arg args {}
                     --argstr uuid #{uuid}
                     '<nixops/eval-deployment.nix>'
                     --eval-only --strict --read-write-mode
                     --arg checkConfigurationOptions false
                     -A info
                     |]

nixBuildMachines ctx exprs uuid names outputPath = Cmd Local [n|
                   env NIX_BUILD_HOOK="$HOME/.nix-profile/libexec/nix/build-remote.pl"
                   NIX_REMOTE_SYSTEMS="$HOME/remote-systems.conf"
                   NIX_CURRENT_LOAD="/tmp/load2"
                   TEST=1
                   nix-build #{nixBaseOptions ctx}
                   --arg networkExprs '#{listToNix exprs}'
                   --arg args {}
                   --argstr uuid #{uuid}
                   --arg names '#{listToNix names}'
                   '<nixops/eval-deployment.nix>'
                   -A machines
                   -o #{outputPath}
                   |]

buildMachines ctx@DeployContext{..}  (State deployment _ exprs machines) = do
  physical <- physicalSpecFile machines
  return $ nixBuildMachines ctx (physical:exprs) (deploymentUuid deployment) (map (T.unpack . resourceName) machines) closuresPath

install DeployContext{..} (State _ _ _ machines) =
    let args m@Resource{..} = (remote m, closuresPath </> (T.unpack resourceName))
        remote m = Remote Nothing (T.unpack $ attr m "publicIpv4")
    in concat [ fmap (uncurry (nixCopyClosureTo sshAuthSock) . args) machines
              , fmap (ssh' sshAuthSock . uncurry nixSetProfile . args) machines
              , fmap (ssh' sshAuthSock . nixSwitchToConfiguration . fst . args) machines
              ]

deploymentInfo :: DeployContext -> State -> IO (Either String Value)
deploymentInfo ctx (State depl _ exprs _) =
    let info = nixDeploymentInfo ctx exprs (deploymentUuid depl)
        in do
          i <- fgconsume info
          return $ nixValue i


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


deploy = do
    let ctx = def :: DeployContext
    s@(State _ resources _ _) <- state ctx

    let keypairs = (\Resource{..} -> resourceType == "ec2-keypair") `filter` resources
    print keypairs
    agentSocket <- setupAgent $ map (flip attr "privateKey") keypairs

    let ctx' = ctx{ sshAuthSock = T.pack agentSocket }
    deployPlan ctx' s >>= mapM_ fgrun

