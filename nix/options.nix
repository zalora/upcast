{ config, pkgs, lib ? pkgs.lib, ... }:

with lib;

let

  cfg = config.deployment;

in

{

  imports = [
    ./ec2.nix
  ];

  options = {

    deployment.targetEnv = mkOption {
      default = "none";
      example = "ec2";
      type = types.str;
      description = ''
        This option is ignored by Upcast. Please do not set it.
      '';
    };

    deployment.targetHost = mkOption {
      type = types.str;
      description = ''
        This option is only used internally by Upcast. Please do not set it.
      '';

    };

  };


}
