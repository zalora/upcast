{ lib }:

let
  inherit (lib) mkOption types id;
in rec {
  inherit (import <upcast/option-types.nix> { inherit lib; }) infra sum submodule;

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

  mkInternalOption = args@{...}: mkOption (args // {
    apply = value: { _internal = (args.apply or id) value; }; # Hack for consumer 'eval-infra'.
    internal = true; # Hack for consumer 'inspectlib'.
  });

  infra-submodule = fn-or-attrs: lib.types.submodule (args@{name,...}: let
    module  = if builtins.isFunction fn-or-attrs then fn-or-attrs args else fn-or-attrs;
    options = (import <upcast/infra-base.nix> args).options // module.options;
  in { config = module.config or {}; inherit options; });
}
