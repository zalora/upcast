{ system ? builtins.currentSystem
, networkExprs
, checkConfigurationOptions ? false
, uuid ? "default"
, args ? {}
}:

let
  lib = import <nixpkgs/lib>;

  eval = lib.evalModules {
    modules = [ ./base.nix rec {
      _file = ./eval-deployment.nix;

      key = _file;

      config = {
        __internal.args = lib.mapAttrs (n: lib.mkDefault) args;

        __internal.check = lib.mkDefault checkConfigurationOptions;

        inherit uuid;

        resources.defaults.machines = [ {
          nixpkgs.system = lib.mkDefault system;
        } ];
      };
    } ] ++ (if lib.isList networkExprs then networkExprs else [ networkExprs ]);
  };
in eval.config.toplevel // { inherit eval; }
