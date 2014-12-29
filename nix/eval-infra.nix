{ expr }:

let

  lib = import ./lib;
  inherit (lib) mapAttrs getAttr evalModules;

  types = import ./infra-types.nix;
  spec = import expr { inherit lib; };

  eval-infra = 
    let
      wrap = m: { _file = expr; imports = [ m ]; };
      eval1 = name: module: type: (evalModules {
        check = true;
        modules = type.baseModules ++ [ (wrap module) ];
        args = {
          inherit name;
          inherit lib;
          infra = eval-infra;
        };
      }).config;
      eval = nset: type: mapAttrs (k: v: eval1 k v type) nset;
      stubs = mapAttrs (k: v: {}) types;
    in stubs // mapAttrs (k: nset: eval nset (getAttr k types)) spec.infra;

in eval-infra
