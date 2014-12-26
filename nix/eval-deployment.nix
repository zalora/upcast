{ exprs }:

let
  lib = import <nixpkgs/lib>;

  base = { config, ... }: rec {
    _file = ./eval-deployment.nix;
    key = _file;

    options = {
      infra =
        let
          topref = { ... }: {
            config.__internal.args = { inherit (config) infra; };
          };
          mkInfraOption = { name, baseModules, ... }: lib.mkOption {
            default = {};
            type = lib.types.attrsOf (lib.types.submodule (baseModules ++ [ topref ]));
          };
          to = name: value: mkInfraOption ({ inherit name; } // value);
        in lib.mapAttrs to (import ./infra-types.nix);
    };
  };

  eval = lib.evalModules {
    check = false;
    modules =
      [ base ] ++
      (if lib.isList exprs then exprs else [ exprs ]);
  };
in {
#  inherit __nixPath;
} // (lib.mapAttrs (n: lib.mapAttrs (n: v: removeAttrs v [ "__internal" ])) eval.config.infra)
