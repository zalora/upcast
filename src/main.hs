{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards #-}

module Main where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text (Text(..))

import Upcast.Interpolate (n)

import Upcast.State
import Upcast.Nix
import Upcast.PhysicalSpec
import Upcast.Command

applyColor index s = "\ESC[1;" ++ color ++ "m" ++ s ++ "\ESC[0m"
  where
    color = show $ (31 + (index `mod` 7))

colorize = applyColor 5


nixops = "/tank/proger/dev/nix/decl/nixops/nixops/../nix"
nixPath = "sources"
stateFile = "deployments.nixops" :: Text
deploymentName = "staging" :: Text

nixBaseOptions = [n|
                 -I #{nixPath}
                 -I nixops=#{nixops} 
                 --option use-binary-cache true
                 --option binary-caches http://hydra.nixos.org
                 --option use-ssh-substituter true
                 --option ssh-substituter-hosts me@node1.example.com
                 --show-trace
                 |]


sshMaster controlPath (Remote key host) =
    Cmd Local [n|ssh -x root@#{host} -S #{controlPath} -M -N -f -oNumberOfPasswordPrompts=0 -oServerAliveInterval=60 -i #{key}|]

sshMasterExit controlPath (Remote _ host) =
    Cmd Local [n|ssh root@#{host} -S #{controlPath} -O exit|]

sshFast controlPath (Cmd (Remote key host) cmd) =
    Cmd Local [n|ssh -oControlPath=#{controlPath} -i #{key} -x root@#{host} -- '#{cmd}'|]

nixCopyClosureTo (Remote key host) path =
    Cmd Local [n|nix-copy-closure --to root@#{host} #{path} --gzip|]

nixBuildMachines exprs uuid names outputPath = Cmd Local [n|
                   nix-build
                   #{nixBaseOptions}
                   --arg networkExprs '#{listToNix exprs}'
                   --arg args {}
                   --argstr uuid #{uuid} <nixops/eval-deployment.nix>
                   --arg names '#{listToNix names}'
                   -A machines
                   -o #{outputPath}
                   |]


attr Resource{..} = fromJust . flip lookup resourceAList
dattr Deployment{..} = fromJust . flip lookup deploymentAList

main = do
  (deployment, res) <- runState stateFile $ do
    d <- onlyDeployment deploymentName >>= deploymentAttrs
    rs <- resources d >>= sequence . fmap resourceAttrs
    return (d, rs)

  let Right exprs = fromNix $ fromJust $ lookup "nixExprs" $ deploymentAList deployment 
  let machines = filter (\Resource{..} -> resourceType == "ec2") res

  physical <- physicalSpecFile machines
  print $ nixBuildMachines (physical:exprs) (deploymentUuid deployment) (map (T.unpack . resourceName) machines) "/tmp/machines"
