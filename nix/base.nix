{ lib, deployment, config, ... }:

let
  mkResourceOption = { name, pluralDisplayName, baseModules }: lib.mkOption {
    description = "The ${pluralDisplayName} in the network.";

    default = {};

    type = lib.types.attrsOf (lib.types.submodule (baseModules ++ deployment.resources.defaults.${name}));
  };

  # !!! Would be preferable to have a mkMap-style property instead
  # of having to have these default options here.
  mkDefaultOption = pluralDisplayName: lib.mkOption {
    description =
      "Extra configuration to apply to each of the ${pluralDisplayName} in the network";

    # !!! Should there be a type corresponding to module syntax?
    type = lib.types.listOf lib.types.unspecified;
  };

  resourceTypes = import ./resource-types.nix;

  pkgs = import <nixpkgs> {};
in {
  options = {
    resources = (lib.mapAttrs (name: value:
      mkResourceOption ({ inherit name; } // value)
    ) resourceTypes) // ({
      defaults = lib.mapAttrs (name: value:
        mkDefaultOption value.pluralDisplayName
      ) resourceTypes;
    });

    toplevel = lib.mkOption {
      description = "The top-level results of the module evaluation.";

      type = lib.types.attrsOf lib.types.unspecified;

      internal = true;
    };

    uuid = lib.mkOption {
      description = "The UUID of the network (set by nixops).";

      type = lib.types.str;

      internal = true;
    };
  };

  config = {
    __internal.args.deployment = config;

    resources.defaults = lib.mapAttrs (name: value:
      if name == "machines"
        then [ ({ name, ... }: {
          deployment.targetHost = lib.mkOverride 900 name;

          __internal.check = lib.mkOverride 900 deployment.__internal.check;

          __internal.args = {
            nodes = deployment.resources.machines;

            resources = deployment.resources;

            modules = [];

            inherit (value) baseModules;
          };
        }) ]
        else [ {
          # backwards compat
          __internal.check = lib.mkDefault false;

          __internal.args = {
            inherit pkgs;

            inherit (deployment) uuid resources;
          };
        } ]
    ) resourceTypes;

    toplevel = {
      info = {
        machines = lib.mapAttrs (n: v': let v = lib.scrubOptionValue v'; in {
          inherit (v.deployment) targetEnv targetHost storeKeysOnMachine keys;
          ec2 = lib.optionalAttrs (v.deployment.targetEnv == "ec2") v.deployment.ec2;
        }) deployment.resources.machines;

        resources = lib.mapAttrs (n:
          lib.mapAttrs (n: v: removeAttrs v [ "__internal" ])
        ) (removeAttrs deployment.resources [ "machines" "defaults" ]);
      };

      machines = { names ? (lib.attrNames deployment.resources.machines) }:
        let machines = lib.filterAttrs (n: v: lib.elem n names) deployment.resources.machines; in
        pkgs.runCommand "nixops-machines" { preferLocalBuild = true; } ''
          mkdir -p $out
          ${toString (lib.attrValues (lib.mapAttrs (n: v: ''
            ln -s ${v.system.build.toplevel} $out/${n}
          '') machines))}
        '';
    };
  };
}
