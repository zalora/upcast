{ config, name, lib, ... }:

with lib;

{
  options = {
    inherit (import ./common.nix { inherit lib; }) accessKeyId region zone vpc cidrBlock;

    name = mkOption {
      default = name;
      type = types.str;
      description = "Name of the subnet.";
    };
  };

  config._type = "ec2-subnet";
}
