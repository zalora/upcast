{ config, pkgs, uuid, name, lib ? pkgs.lib, ... }:

with lib;

{

  options = {

    region = mkOption {
      type = types.str;
      description = "Amazon EC2 region.";
    };

    accessKeyId = mkOption {
      default = "";
      type = types.str;
      description = "The AWS Access Key ID.";
    };

  };

  config._type = "ec2-subnet";

}
