{ expr }:

let
  lib = import <upcast/lib>;
  inherit (lib) evalModules unique sort;
  inherit (import <upcast/extralib.nix>) collect filterAttrsRecursive;

  spec = let x = import expr; in if builtins.isFunction x then (x { inherit lib; }) else x;

  eval-infra = let
    eval1 = module: filterAttrsRecursive (x: ! x ? "_internal") (evalModules {
      args = { infra = eval-infra; inherit lib; };
      modules = [
        { _file = expr; imports = [module]; }
        { imports = [<upcast/infra-types.nix>]; }
      ];
    }).config;
    out = if (spec.upcast-eval-infra or true)
            then eval1 spec.infra
            else spec.infra;
    meta = {
      realm-name = spec.realm-name or "";
      regions = unique (sort (x: y: x == y) (map (x: x.region) (collect (as: as ? region) out)));
    };
  in meta // out;
in if spec ? infra then eval-infra else throw "expression ${expr} doesn't contain any infra"
