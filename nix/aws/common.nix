{ lib }:

let
  inherit (lib) mkOption types;
in rec {
  inherit (import <upcast/option-types.nix> { inherit lib; }) infra sum;

  accessKeyId = mkOption {
    default = "default";
    type = types.str;
    description = "The AWS Access Key ID (ignored by Upcast).";
  };

  region = mkOption {
    type = types.str;
    description = "Amazon EC2 region.";
  };

  zone = mkOption {
    example = "us-east-1c";
    type = types.str;
    description = "Amazon EC2 availability zone.";
  };

  cidrBlock = mkOption {
    type = types.str;
    default = "";
  };

  vpc = mkOption {
    type = infra "ec2-vpc";
    apply = x: if builtins.isString x then x else x._name;
    description = "VPC which contains the infra.";
  };

  subnet = mkOption {
    type = infra "ec2-subnet";
    apply = x: if builtins.isString x then x else x._name;
    description = "EC2 VPC subnet ID.";
  };

  nullOr = option: mkOption ({
    default = null;
    type = types.nullOr (option.type);
    apply = x: if x == null then null else option.apply x;
  });
}
