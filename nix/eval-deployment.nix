{ system ? builtins.currentSystem
, networkExprs
, checkConfigurationOptions ? false
, uuid ? "new-upcast-deployment"
, args ? {}
, internal ? {}
}:

let
  lib = import <nixpkgs/lib>;

  eval = lib.evalModules {
    modules = [ ./base.nix rec {
      _file = ./eval-deployment.nix;

      key = _file;

      config = {
        __internal = {
          args = lib.mapAttrs (n: lib.mkDefault) args;
          check = lib.mkDefault checkConfigurationOptions;
        } // internal;

        inherit uuid;

        resources.defaults.machines = [ {
          nixpkgs.system = lib.mkDefault system;
        } ];
      };
    } ] ++ (if lib.isList networkExprs then networkExprs else [ networkExprs ]);
  };
in eval.config.toplevel // { inherit eval; inherit __nixPath; }
