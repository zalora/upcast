{ config, pkgs, lib ? pkgs.lib, ... }:

with lib;

{
  # FIXME: move to nixpkgs/lib/types.nix.
  union = t1: t2: mkOptionType {
    name = "${t1.name} or ${t2.name}";
    check = x: t1.check x || t2.check x;
    merge = mergeOneOption;
  };

  resource = type: mkOptionType {
    name = "resource of type ‘${type}’";
    check = x: x._type or "" == type;
    merge = mergeOneOption;
  };

  ec2-machine = mkOptionType {
    name = "ec2 machine";
    check = x: builtins.hasAttr "ec2" x.deployment;
    merge = mergeOneOption;
  };
}
