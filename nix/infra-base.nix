{ name, lib, ... }:

with lib;

{
  options = {
    # Pass-through of the infra name.
    _name = mkOption {
      default = name;
      visible = false;
      type = types.str;
      description = "Name of the infra.";
    };

    # Type of the infra (for dynamic type checks).
    _type = mkOption {
      default = "unknown";
      type = types.str;
      visible = false;
      description = "Type of the infra.";
    };
  };
}
