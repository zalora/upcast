{ lib, ... }:

with lib;

let
  ec2 = import ./ec2-info.nix;
in
{
  resources.machines.node = { ... }: {
    imports = [
      <upcast/env-ec2.nix>
    ];

    deployment.ec2 = ec2.args // {
      instanceType = "m3.medium";
      ami = ec2.amis."${ec2.args.region}".default;
    };
    deployment.nix = true;
  };
}
