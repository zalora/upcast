{ config, pkgs, uuid, name, lib ? pkgs.lib, ... }:

with lib;
let inherit (import ./lib.nix { inherit config pkgs lib; }) union resource ec2-machine; in

{

  options = {

    name = mkOption {
      example = "the-best-elb";
      default = name;
      type = types.str;
      description = "Unique name of the ELB.";
    };

    accessKeyId = mkOption {
      type = types.str;
      description = "The AWS Access Key ID.";
    };

    region = mkOption {
      example = "us-east-1";
      type = types.str;
      description = "Amazon EC2 region.";
    };

    subnetsPerAZ = mkOption {
      type = types.attrsOf (union types.str (resource "ec2-subnet"));
      default = {};
      apply = mapAttrs (name: value: { inherit name; value = if builtins.isString value then value else value.name; });
    };

    machines = mkOption {
      type = types.listOf (union types.str ec2-machine);
      default = [];
      apply = map (x: if builtins.isString x then x else x.deployment.targetHost);
    };

    listeners = mkOption {
      type = types.listOf (types.submodule ({ lib, name, ... }: with lib; {
        options = {
          lbPort = mkOption { type = types.int; default = 80; };
          lbProtocol = mkOption { type = types.string; default = "http"; };
          instancePort = mkOption { type = types.int; default = 80; };
          instanceProtocol = mkOption { type = types.string; default = "http"; };
          sslCertificateId = mkOption { type = types.string; default = ""; };
        };
      }));
      default = [
        { lbPort = 80; lbProtocol = "http"; instancePort = 80; instanceProtocol = "http"; }
      ];
    };

    securityGroups = mkOption {
      example = [ "my-group" "my-other-group" ];
      type = types.listOf (union types.str (resource "ec2-security-group"));
      apply = map (x: if builtins.isString x then x else x.name);
      description = "Security groups for the ELB withing its VPC";
      default = [];
    };

    internal = mkOption {
      type = types.bool;
      default = false;
    };

    accessLog = mkOption {
      type = types.submodule ({ lib, name, ... }: with lib; {
        options = {
          enable = mkOption { type = types.bool; default = false; };
          emitInterval = mkOption { type = types.int; default = 60; };
          s3BucketName = mkOption { type = types.string; default = ""; };
          s3BucketPrefix = mkOption { type = types.string; default = ""; };
        };
      });
      default = {
        enable = false;
        emitInterval = 60;
      };
    };

    connectionDraining = mkOption {
      type = types.submodule ({ lib, name, ... }: with lib; {
        options = {
          enable = mkOption { type = types.bool; default = true; };
          timeout = mkOption { type = types.int; default = 300; };
        };
      });
      default = {
        enable = true;
        timeout = 300;
      };
    };

    crossZoneLoadBalancing = mkOption {
      type = types.bool;
      default = true;
    };

  };

  config._type = "elb";
}
