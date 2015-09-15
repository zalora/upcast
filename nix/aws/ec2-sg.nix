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
      type = types.listOf (common.submodule ({ lib, name, ... }: {
        options = {
          protocol = mkOption {
            default = "tcp";
            description = "The protocol (tcp, udp, or icmp) that this rule describes.";
            type = types.str;
          };

          fromPort = mkOption {
            default = null;
            description = ''
              The start of port range for the TCP and UDP protocols, or an ICMP
              type number. For the ICMP type number, use -1 to specify all ICMP
              types.
            '';
            type = types.uniq (types.nullOr types.int);
          };

          toPort = mkOption {
            default = null;
            description = ''
              The end of port range for the TCP and UDP protocols, or an ICMP
              code number. For the ICMP code number, use -1 to specify all ICMP
              codes for the ICMP type.
            '';
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
        config._type = "rule";
      }));
      description = "The security group's rules.";
      default = [];
    };
  };

  config._type = "ec2-sg";
}
