{ config, name, lib, ... }:

with lib;

{
  options = {
    inherit (import ./common.nix { inherit lib; }) accessKeyId region cidrBlock;
  };

  config._type = "ec2-vpc";
}
