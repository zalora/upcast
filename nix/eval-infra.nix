{ expr }:

let

  lib = import ./lib;
  inherit (lib) mapAttrs getAttr evalModules;

  types = import ./infra-types.nix;
  spec = import expr { inherit lib; };

  eval-infra = 
    let
      eval1 = name: module: type: (evalModules {
        check = false;
        modules = type.baseModules ++ [ module ];
        args = {
          inherit name;
          pkgs = {};
          inherit lib;
          infra = eval-infra;
        };
      }).config;
      eval = nset: type: mapAttrs (k: v: eval1 k v type) nset;
    in mapAttrs (k: nset: eval nset (getAttr k types)) spec.infra;

in eval-infra
