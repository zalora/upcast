{ expr }:

let

  lib = import ./lib;
  inherit (lib) mapAttrs getAttr evalModules sort unique;
  extralib = import ./extralib.nix;
  inherit (extralib) collect;

  types = import ./infra-types.nix;
  spec =
    let
      x = import expr;
    in if builtins.isFunction x then (x { inherit lib; }) else x;

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
      out = mapAttrs (k: nset: eval nset (getAttr k types)) spec.infra;
      meta = {
        realm-name = spec.realm-name or "";
        regions = unique (sort (x: y: x == y) (map (x: x.region) (collect (as: as ? region) out)));
      };
    in meta // stubs // out;

in if spec ? infra then eval-infra else throw "expression ${expr} doesn't contain any infra"
