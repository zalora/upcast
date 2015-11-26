{ infra, config, lib, ... }: let
  inherit (lib) types mkOption mapAttrs mkOverride;
  common = (import ./common.nix { inherit lib; });
  inherit (common) infra-submodule mkInternalOption;
in {
  options = {
    autoscaling-group = mkOption {
      type = types.attrsOf (infra-submodule (args@{ name, ... }: {
        options = {
          inherit (import ./common.nix { inherit lib; }) accessKeyId region zone;
          name = mkOption {
            default = name;
            type = types.str;
            description = ''
              Name of the auto-scaling group.
            '';
          };
          launchConfiguration = mkOption {
            type = common.infra "launch-configuration";
            description = ''
              Launch configuration describing the template for spawned instances.
            '';
          };
          minSize = mkInternalOption {
           type = types.int;
           description = ''
             Minimum number of instances admitted in the ASG.
           '';
          };
          maxSize = mkInternalOption {
           type = types.int;
           description = ''
             Largest number of instances admitted in the ASG.
           '';
          };
          subnets = mkOption {
            type = types.nonEmptyListOf (common.infra "ec2-subnet");
            description = ''
              Subnets to distribute instances between.
            '';
          };
          loadBalancers = mkOption {
            default = [];
            type = types.listOf (common.infra "elb");
            description = ''
              Load balancers in which to assign spawned instances.
            '';
          };
          healthcheckType = mkOption {
            type = types.nullOr (types.enum ["EC2" "ELB"]);
            default = null;
            description = ''
              The service to use for instance healthchecks. 'EC2' means instance status
              checks, 'ELB' means healthchecks of all associated load balancers.
            '';
          };
          healthcheckGracePeriod = mkOption {
            type = types.int;
            default = 600;
            description = ''
              Time between an instance coming into service, and auto scaling testing
              its health. Before this time, any health check failures are ignored.
            '';
          };
          tag = mkOption {
            default = [];
            type = types.listOf (types.submodule ({ ... }: {
              options = {
                key = mkOption {
                  type = types.str;
                };
                value = mkOption {
                  type = types.str;
                };
                propagateAtLaunch = mkOption {
                  type = types.bool;
                  description = ''
                    Whether this tag should also be attached to instances spawned by
                    the auto-scaling group.
                  '';
                };
              };
            }));
            description = ''
              Tags to attach to the auto-scaling group.
            '';
          };
        };
        config._type = "autoscaling-group";
      }));
      default = {};
    };
    autoscaling-options = mkOption {
      type = types.attrsOf (infra-submodule (args@{...}: {
        options = {
          autoScalingGroup = mkOption {
            type = common.infra "autoscaling-group";
          };
          minSize = mkOption {
           type = types.int;
           description = ''
             Minimum number of instances admitted in the ASG.
           '';
          };
          maxSize = mkOption {
           type = types.int;
           description = ''
             Largest number of instances admitted in the ASG.
           '';
          };
        };
        config._type = "autoscaling-options";
      }));
    };
  };
  config = {
    autoscaling-options = mkOverride 0 (mapAttrs (k: v: {
      autoScalingGroup = infra.autoscaling-group.${k};
      minSize = v.minSize._internal;
      maxSize = v.maxSize._internal;
    }) config.autoscaling-group);
  };
}
