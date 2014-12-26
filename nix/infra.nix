{ config, pkgs, name, lib ? pkgs.lib, ... }:

with lib;


{

  options = {

    # Pass-through of the infra name.
    _name = mkOption {
      default = name;
      visible = false;
      description = "Name of the infra.";
    };

    # Type of the infra (for dynamic type checks).
    _type = mkOption {
      default = "unknown";
      visible = false;
      description = "Type of the infra.";
    };

  };

}

