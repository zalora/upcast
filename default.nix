let
   nixpkgs = (import <nixpkgs> {}).fetchgit {
    url = "git://github.com/zalora/nixpkgs.git";
    rev = "e6ac094dc5c3470571292744c4b007d7da40256f";
    sha256 = "1dcd4cc2c769f285cbd278ff35222ff49d0e764d7043f843a4e90e870b82af40";
  };
in
{ system ? builtins.currentSystem
, pkgs ? import nixpkgs { inherit system; }
, src ? builtins.filterSource (path: type: type != "unknown" && baseNameOf path != ".git" && baseNameOf path != "result") ./.
, haskellPackages ? pkgs.haskellPackages_ghc782
}:
haskellPackages.buildLocalCabal src "upcast"
