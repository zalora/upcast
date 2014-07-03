{ config, pkgs, uuid, name, lib ? pkgs.lib, ... }:

with lib;

{

  options = {

    subnet = mkOption {
      default = name;
      type = types.str;
      description = "EC2 VPC subnet";
    };

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
