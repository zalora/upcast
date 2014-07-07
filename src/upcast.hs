{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards #-}

module Main where

import System.FilePath.Posix ((</>))
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text (Text(..))

import Data.Default

import Upcast.Interpolate (n)

import Upcast.State
import Upcast.Nix
import Upcast.PhysicalSpec
import Upcast.Command
import Upcast.Temp
import Upcast.Aws
import Upcast.ATerm (valueForKeyPathS)

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

nixBaseOptions DeployContext{..} = [n|
                 -I #{nixPath}
                 -I nixops=#{nixops}
                 --option use-binary-cache true
                 --option binary-caches http://hydra.nixos.org
                 --option use-ssh-substituter true
                 --option ssh-substituter-hosts me@node1.example.com
                 --show-trace
                 |]

sshAgent socket = Cmd Local [n|ssh-agent -a #{socket}|]
sshAddKey socket key = Cmd Local [n|echo '#{key}' | env SSH_AUTH_SOCK=#{socket} SSH_ASKPASS=/usr/bin/true ssh-add -|]
sshListKeys socket = Cmd Local [n|env SSH_AUTH_SOCK=#{socket} ssh-add -l|]

nixCopyClosureTo sshAuthSock (Remote _ host) path =
    Cmd Local [n|env SSH_AUTH_SOCK=#{sshAuthSock} nix-copy-closure --to root@#{host} #{path} --gzip|]

nixCopyClosureToFast controlPath (Remote key host) path =
    Cmd Local [n|env NIX_SSHOPTS="-i #{key} -S #{controlPath}" nix-copy-closure --to root@#{host} #{path} --gzip|]

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
                   nix-build #{nixBaseOptions ctx}
                   --arg networkExprs '#{listToNix exprs}'
                   --arg args {}
                   --argstr uuid #{uuid}
                   --arg names '#{listToNix names}'
                   '<nixops/eval-deployment.nix>'
                   -A machines
                   -o #{outputPath}
                   |]

nixSetProfile remote closure = Cmd remote [n|
                                  nix-env -p /nix/var/nix/profiles/system --set "#{closure}"
                                  |]

nixSwitchToConfiguration remote = Cmd remote [n|
                                  env NIXOS_NO_SYNC=1 /nix/var/nix/profiles/system/bin/switch-to-configuration switch
                                  |]

-- nixTrySubstitutes remote closure =
               -- closure = subprocess_check_output(["nix-store", "-qR", path]).splitlines()
               -- self.run_command("nix-store -j 4 -r --ignore-unknown " + ' '.join(closure), check=False)


attr Resource{..} = fromJust . flip lookup resourceAList
dattr Deployment{..} = fromJust . flip lookup deploymentAList

data State = State Deployment [Resource] [String] [Resource]
           deriving (Show)

state DeployContext{..} = do
  (deployment, res) <- runState stateFile $ do
    d <- onlyDeployment deploymentName >>= deploymentAttrs
    rs <- resources d >>= sequence . fmap resourceAttrs
    return (d, rs)

  let Right exprs = fromNix $ fromJust $ lookup "nixExprs" $ deploymentAList deployment
  let machines = filter (\Resource{..} -> resourceType == "ec2") res

  return $ State deployment res exprs machines

deploymentInfo ctx (State deployment _ exprs _) =
    let info = nixDeploymentInfo ctx (exprs) (deploymentUuid deployment)
        in do
          i <- fgconsume info
          return $ nixValue i

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

ssh' :: Text -> Command Remote -> Command Local
ssh' sshAuthSock (Cmd (Remote _ host) cmd) =
    Cmd Local [n|env SSH_AUTH_SOCK=#{sshAuthSock} ssh -x root@#{host} -- '#{cmd}'|]


deployPlan ctx@DeployContext{..} s = do
    Right info <- deploymentInfo ctx s
    print $ fromJust $ valueForKeyPathS "machines.db1.ec2.subnet" info
    print $ fromJust $ valueForKeyPathS "resources.subnets" info
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
    agentSocket <- randomTempFileName "ssh-agent.sock."
    spawn $ sshAgent agentSocket
    mapM_ (fgrun . sshAddKey agentSocket) $ map (flip attr "privateKey") keypairs
    fgrun $ sshListKeys agentSocket

    let ctx' = ctx{ sshAuthSock = T.pack agentSocket }
    deployPlan ctx' s >>= mapM_ fgrun

main = planOnly
