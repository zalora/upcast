exprs:
let
  inherit (import <nixpkgs/lib>) mkDefault evalModules isList;

  pkgs-mod = rec {
    _file = ./default.nix;
    key = _file;
    config = {
      nixpkgs.system = mkDefault "x86_64-linux";
    };
  };

  eval = evalModules {
    modules =
      [ pkgs-mod ] ++
      (import <nixpkgs/nixos/modules/module-list.nix>) ++
      (if isList exprs then exprs else [ exprs ]);
  };
in {
  inherit (eval) config options;
}
