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
    example = "us-east-1";
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
    description = "VPC which contains the infra.";
    apply = x: x;
  };

  subnet = mkOption {
    type = infra "ec2-subnet";
    description = "EC2 VPC subnet ID.";
    apply = x: x;
  };

  nullOr = option: mkOption {
    default = null;
    type = types.nullOr (option.type or option);
    apply = x: if x == null then null else option.apply x;
  };
}
