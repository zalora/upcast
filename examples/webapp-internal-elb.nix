{ lib, ... }:

with lib;

let
  accessKeyId = "default";
  region = "ap-southeast-1";
  securityGroups = [ "sg-eeeeeeee" ];
  subnet = "subnet-eeeeeeee";

  ec2-args = {
    accessKeyId = "default";

    inherit region;
    zone = "ap-southeast-1b";
    keyPair = "super-shared-keypair";

    inherit subnet;
    inherit securityGroups;

    instanceProfileARN = "arn:aws:iam::000000000000:instance-profile/profile-1";
  };

  webapp = instanceType: { resources, ... }: {
    deployment.ec2 = ec2-args // {
      inherit instanceType;
      ami = "ami-example";
    };
    deployment.nix = false;
  };
in
{
  resources.machines.node1 = webapp "m3.medium";
  resources.machines.node2 = webapp "m3.medium";

  resources.elbs.node-balancer = { resources, ... }: {
    inherit accessKeyId region securityGroups;
    subnets = [ subnet ];
    machines = with resources.machines; [ node1 node2 ];
    internal = true;

    route53Aliases."app.internal.doge-enterprises.com".zoneId = "ZEEEEEEEEEEEEE";
  };
}
