{ config, pkgs, name, lib ? pkgs.lib, ... }:

with lib;
let inherit (import ./lib.nix { inherit lib; }) union infra; in

{

  options = {

    region = mkOption {
      type = types.str;
      description = "Amazon EC2 region.";
    };

    zone = mkOption {
      example = "us-east-1c";
      type = types.str;
      description = "Amazon EC2 availability zone";
    };

    accessKeyId = mkOption {
      default = "";
      type = types.str;
      description = "The AWS Access Key ID.";
    };

    cidrBlock = mkOption {
      type = types.str;
      default = name;
    };

    vpc = mkOption {
      type = union types.str (infra "ec2-vpc");
      apply = x: if builtins.isString x then x else x._name;
      description = "VPC which contains the subnet.";
    };

  };

  config._type = "ec2-subnet";

}
