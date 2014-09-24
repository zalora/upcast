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
    deployment.targetHost = mkOption {
      type = types.str;
      description = ''
        This option is only used internally by Upcast. Please do not set it.
      '';

    };

    deployment.nix = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to assume Nix builds on this instance.
      '';
    };
  };
}
