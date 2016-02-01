{ infra, config, lib, ... }: let
  inherit (lib) types mkOption mapAttrs mkOverride;
  common = (import ./common.nix { inherit lib; });
  inherit (common)  mkInternalOption infra-submodule;
in {
  options = {
    launch-configuration = mkOption {
      type = types.attrsOf (infra-submodule (args@{ name, ... }: {
        options = {
          inherit (import ./common.nix { inherit lib; }) accessKeyId region zone;

          prefix = mkOption {
            default = name;
            type = types.str;
            description = ''
              Prefix of the launch configuration's name. Upcast appends a UUID to
              support rolling ASG deployments, as launch configurations can't be
              updated after creation.
            '';
          };
          ami = mkOption {
            type = types.str;
            description = ''
              Identifier of the Amazon Machine Image instances are launched with. Be
              careful to match this with 'ebsOptimized' and 'instanceType'.
            '';
          };
          instanceType = mkOption {
            type = types.str;
            description = ''
              The size of instances to launch in this configuration. Be careful to match
              this with 'ami' and 'ebsOptimized'.
            '';
          };
          ebsOptimized = mkOption {
            default = false;
            type = types.bool;
            description = ''
              Whether EC2 instances should be EBS Optimized. This implicitly
              requires a compatible 'instanceType' and 'ami'.
            '';
          };
          instanceProfileARN = mkOption {
            default = null;
            type = types.nullOr types.str;
            description = ''
              ARN of the IAM instance profile to associate with launched
              instances.
            '';
          };
          keyName = mkOption {
            default = null;
            type = types.nullOr (common.infra "ec2-keypair");
            description = ''
              Name identifying a SSH public key made available over the metadata
              address to instances belonging to this launch configuration. Null means
              no keypair.
            '';
          };
          securityGroups = mkOption {
            type = types.listOf (common.infra "ec2-sg");
            description = ''
              List of security groups launched instances are associated with. Empty
              means no security groups.
            '';
          };
          associatePublicIPAddress = mkOption {
            type = types.bool;
            default = false;
            description = ''
              Whether launched instances should be assigned public IP addresses.
            '';
          };
        };
        config._type = "launch-configuration";
      }));
      default = {};
    };
  };
}
