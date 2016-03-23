{ lib ? import <upcast/lib>, ... }:
let
  inherit (lib) attrValues mapAttrs;
  modernise-infra = k: v: { ... }: let
    inherit (lib) mkOption types;
    inherit (import <upcast/aws/common.nix> { inherit lib; }) infra-submodule;
  in {
    options = {
      ${k} = mkOption {
        type = types.attrsOf (infra-submodule (import v));
        default = {};
      };
    };
  };
in {
  imports = ([
    <upcast/aws/ec2-sg.nix> # -> {ec2-sg, ec2-sg-ruleset}
    <upcast/aws/elb.nix> # -> {elb, elb-instance-set}
  ] ++ attrValues (mapAttrs modernise-infra {
    ebs          = <upcast/aws/ebs-volume.nix>;
    ec2-keypair  = <upcast/aws/ec2-keypair.nix>;
    ec2-instance = <upcast/aws/ec2-instance.nix>;
    ec2-subnet   = <upcast/aws/ec2-subnet.nix>;
    ec2-vpc      = <upcast/aws/ec2-vpc.nix>;
  }));
}
