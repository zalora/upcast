{ config, name, lib, ... }:

with lib;
let
  common = import ./common.nix { inherit lib; };
in
{
  options = {
    inherit (common) accessKeyId region;

    name = mkOption {
      default = "charon-${name}";
      type = types.str;
      description = "Name of the security group.";
    };

    description = mkOption {
      default = "${name}";
      type = types.str;
      description = "Informational description of the security group.";
    };

    vpc = common.nullOr common.vpc;

    rules = mkOption {
      type = types.listOf (types.submodule ({ lib, name, ... }: {
        options = {
          protocol = mkOption {
            default = "tcp";
            description = "The protocol (tcp, udp, or icmp) that this rule describes.";
            type = types.str;
          };

          fromPort = mkOption {
            default = null;
            description = "The bottom of the allowed port range for this rule (TCP/UDP only).";
            type = types.uniq (types.nullOr types.int);
          };

          toPort = mkOption {
            default = null;
            description = "The top of the allowed port range for this rule (TCP/UDP only).";
            type = types.uniq (types.nullOr types.int);
          };

          typeNumber = mkOption {
            default = null;
            description = "ICMP type number (ICMP only, -1 for all).";
            type = types.uniq (types.nullOr types.int);
          };

          codeNumber = mkOption {
            default = null;
            description = "ICMP code number (ICMP only, -1 for all).";
            type = types.uniq (types.nullOr types.int);
          };

          sourceGroupOwnerId = mkOption {
            default = null;
            description = "The AWS account ID that owns the source security group.";
            type = types.uniq (types.nullOr types.str);
          };

          sourceGroupName = mkOption {
            default = null;
            description = "The name of the source security group (if allowing all instances in a group access instead of an IP range).";
            type = types.uniq (types.nullOr types.str);
          };

          sourceIp = mkOption {
            default = null;
            description = "The source IP range (CIDR notation).";
            type = types.uniq (types.nullOr types.str);
          };
        };
      }));
      description = "The security group's rules.";
      default = {};
    };
  };

  config._type = "ec2-sg";
}
