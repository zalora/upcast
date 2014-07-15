{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RecordWildCards, NamedFieldPuns #-}

module Upcast.PhysicalSpec (
  physicalSpecFile
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromJust)

import Data.List (intercalate)

import Upcast.Interpolate (nl)
import Upcast.Nix
import Upcast.Temp

import Upcast.State (Resource(..))
import Upcast.Resource (Machine(..))

class EC2Hostable a where toEC2Host :: a -> EC2Host

instance EC2Hostable Resource where toEC2Host = resourceToEC2Host
instance EC2Hostable Machine where toEC2Host = machineToEC2Host

physicalSpecFile :: EC2Hostable a => [a] -> IO FilePath
physicalSpecFile rs = writeTempFile "physical.nix" (physicalSpec rs)

physicalSpec :: EC2Hostable a => [a] -> String
physicalSpec rs = physicalSpecTemplate $ fmap ec2HostTemplate $ ec2Hosts rs

ec2Hosts :: EC2Hostable a => [a] -> [EC2Host]
ec2Hosts = interlink . fmap toEC2Host

data EC2Host = EC2Host
             { hostname :: String
             , kernelModules :: [String]
             , extraHosts :: [String]
             , privateIPv4 :: String
             , publicIPv4 :: String
             , knownHosts :: [String]
             , instanceId :: String
             , publicHostKey :: Maybe String
             } deriving (Show, Eq)


resourceToEC2Host Resource{..} =
    EC2Host (T.unpack resourceName)
            []
            []
            (attr "privateIpv4")
            (attr "publicIpv4")
            []
            (attr "vmId")
            (Just $ attr "ec2.publicHostKey")
  where
    attr = T.unpack . fromJust . flip lookup resourceAList

machineToEC2Host Machine{..} =
    EC2Host (T.unpack m_hostname)
            []
            []
            (T.unpack m_privateIp)
            (T.unpack m_publicIp)
            []
            (T.unpack m_instanceId)
            Nothing

interlink hosts = zipWith link hosts $ others hosts
  where
    link host neighbours = host { knownHosts = map knownHostTemplate neighbours
                                , extraHosts = map extraHostsTemplate neighbours
                                }

    others xs = take len $ go $ cycle xs
        where
          len = length xs

          go l@(x:xs) = take (len - 1) xs : go xs
          go [] = []


physicalSpecTemplate machines = [nl|
    {
      resources.machines = {
        #{intercalate "\n" machines}
      };
    }
  |]

ec2HostTemplate EC2Host{..} = [nl|
    #{hostname} = { config, pkgs, ... }: {
      config = {
        boot.kernelModules = #{listToNix kernelModules};
        networking = {
          extraHosts = ''
            #{intercalate "\n" extraHosts}
          '';
          firewall.trustedInterfaces = [];
          privateIPv4 = "#{privateIPv4}";
          publicIPv4 = "#{publicIPv4}";
        };
        services.openssh.knownHosts = {
          #{intercalate "\n" knownHosts}
        };
      };
      imports = [
        {
          deployment.ec2 = {
            blockDeviceMapping = {};
            instanceId = "#{instanceId}";
          };
          require = [
            <nixpkgs/nixos/modules/virtualisation/amazon-config.nix>
          ];
        }
      ];
    };
  |]

aliases EC2Host{..} = [hostname, hostname ++ "-encrypted", hostname ++ "-unencrypted"]
extraHostsTemplate EC2Host{..} = [nl|#{privateIPv4} #{hostname} #{hostname}-unencrypted|]

knownHostTemplate host@EC2Host{publicHostKey = Nothing} = [nl||]
knownHostTemplate host@EC2Host{hostname, publicHostKey = (Just k)} = [nl|
      #{hostname} = {
        hostNames = #{listToNix $ aliases host};
        publicKey = "#{k}";
      };
    |]

try = physicalSpecTemplate $ map ec2HostTemplate [staging0, staging1, db1]
  where
    base = EC2Host
             { hostname = "base"
             , kernelModules = []
             , extraHosts = []
             , privateIPv4 = "10.1.1.1"
             , publicIPv4 = "1.1.1.1"
             , knownHosts = []
             , instanceId = "i-babadeda"
             , publicHostKey = Just "sup"
             }
    staging0 = base {
                     hostname = "staging0"
                   , knownHosts = map knownHostTemplate [staging1, db1]
                   , extraHosts = map extraHostsTemplate [staging1, db1]
                   }
    staging1 = base {
                     hostname = "staging1"
                   , knownHosts = map knownHostTemplate [staging0, db1]
                   , extraHosts = map extraHostsTemplate [staging0, db1]
                   }
    db1 = base {
                     hostname = "db1"
                   , knownHosts = map knownHostTemplate [staging0, staging1]
                   , extraHosts = map extraHostsTemplate [staging0, staging1]
                   }
