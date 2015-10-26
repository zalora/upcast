{ infra, config, lib, ... }: let
  inherit (lib) types mkOption mapAttrs mkOverride;
  common = (import ./common.nix { inherit lib; });
  inherit (common)  mkInternalOption infra-submodule;
  type-of-rules = types.listOf (infra-submodule ({ ... }: {
    options = {
      protocol = mkOption {
        default = "tcp";
        description = ''
          The protocol (tcp, udp, or icmp) that this rule describes.
        '';
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
        description = ''
          The AWS account ID that owns the source security group.
        '';
        type = types.uniq (types.nullOr types.str);
      };
      sourceGroupName = mkOption {
        default = null;
        description = ''
          The name of the source security group (if allowing all instances in
          a group access instead of an IP range).
        '';
        type = types.uniq (types.nullOr types.str);
      };
      sourceIp = mkOption {
        default = null;
        description = ''
          The source IP range (CIDR notation).
        '';
        type = types.uniq (types.nullOr types.str);
      };
    };
    config._type = lib.mkForce "rule"; # XXX: Why is 'mkForce' necessary?
  }));
in {
  options = {
    ec2-sg = mkOption {
      type = types.attrsOf (infra-submodule (args@{ name, ... }: {
        options = {
          inherit (common) accessKeyId region;
          name = mkOption {
            type = types.str;
            default = "charon-${name}";
            description = "Name of the security group";
          };
          description = mkOption {
            type = types.str;
            default = "${name}";
            description = "Informational description of the security group.";
          };
          vpc = common.nullOr common.vpc;
          rules = mkInternalOption {
            description = "The security group's rules.";
            type = type-of-rules;
            default = [];
          };
        };
        config._type = "ec2-sg";
      }));
      default = {};
    };
    ec2-sg-ruleset = mkOption {
      type = types.attrsOf (infra-submodule (args@{...}: {
        options = {
          securityGroup = mkOption {
            default = "";
            type = common.infra "ec2-sg";
          };
          rules = mkOption {
            description = "The security group's rules.";
            type = type-of-rules;
          };
        };
        config._type = "ec2-sg-ruleset";
      }));
    };
  };
  config = {
    ec2-sg-ruleset = mkOverride 0 ( # can't touch this
      mapAttrs (k: v: {
        securityGroup = infra.ec2-sg.${k};
        rules = v.rules._internal;
      }) config.ec2-sg
    );
  };
}
