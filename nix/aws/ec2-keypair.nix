{ config, name, lib, ... }:

with lib;

{
  options = {
    inherit (import ./common.nix { inherit lib; }) accessKeyId region;

    name = mkOption {
      default = "charon-${name}";
      type = types.str;
      description = "Name of the EC2 key pair.";
    };

    privateKeyFile = mkOption {
      default = "";
      type = types.str;
      description = "Key to import";
    };
  };

  config._type = "ec2-keypair";
}
