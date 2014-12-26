{ config, name, lib, ... }:

with lib;

{
  options = {
    inherit (import ./common.nix { inherit lib; }) accessKeyId region zone vpc cidrBlock;
  };

  config._type = "ec2-subnet";
}
