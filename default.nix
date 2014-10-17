let
   nixpkgs = (import <nixpkgs> {}).fetchgit {
    url = "git://github.com/NixOS/nixpkgs.git";
    rev = "5cac6895da590a6c9bf55d50d01247a1a14a5d8d";
    sha256 = "22414544b7706db2344fbb21b2bd6d9e021e1a14307cbd4c1a6e7f73cfb4937d";
  };
in
{ system ? builtins.currentSystem
, pkgs ? import nixpkgs { inherit system; }
, name ? "upcast"
, src ? builtins.filterSource (path: type: type != "unknown" && baseNameOf path != ".git" && baseNameOf path != "result") ./.
, haskellPackages ? pkgs.haskellPackages_ghc783
}:
with pkgs.lib;

let
  unsafeDerivation = args: derivation ({
    PATH = builtins.getEnv "PATH";
    system = builtins.currentSystem;
    builder = ./bootstrap/proxy-bash.sh;
    preferLocalBuild = true;
    __noChroot = true;
  } // args);

  shell = name: command: unsafeDerivation {
    inherit name;
    args = ["-c" command];
  };

  # Build a cabal package given a local .cabal file
  buildLocalCabalWithArgs = { src
                            , name
                            , args ? {}
                            , cabalDrvArgs ? { jailbreak = true; }
                            # for import-from-derivation, want to use current system
                            , nativePkgs ? import pkgs.path {}
                            }: let

    cabalExpr = shell "${name}.nix" ''
      export HOME="$TMPDIR"
      ${let c2n = builtins.getEnv "OVERRIDE_cabal2nix";
        in if c2n != "" then c2n
           else "${nativePkgs.haskellPackages.cabal2nix}/bin/cabal2nix"} \
        ${src + "/${name}.cabal"} --sha256=FILTERME \
          | grep -v FILTERME | sed \
            -e 's/{ cabal/{ cabal, cabalInstall, cabalDrvArgs ? {}, src/' \
            -e 's/cabal.mkDerivation (self: {/cabal.mkDerivation (self: cabalDrvArgs \/\/ {/' \
            -e 's/buildDepends = \[/buildDepends = \[ cabalInstall/' \
            -e 's/pname = \([^$]*\)/pname = \1  inherit src;/'  > $out
    '';
  in haskellPackages.callPackage cabalExpr ({ inherit src cabalDrvArgs; } // args);

  aws = haskellPackages.callPackage ./nixpkgs/aws.nix {};
in

buildLocalCabalWithArgs {
  inherit src name;
  args = {
    inherit aws;
    vkAwsRoute53 = haskellPackages.callPackage ./nixpkgs/vk-aws-route53.nix { inherit aws; };
    awsEc2 = haskellPackages.callPackage ./nixpkgs/aws-ec2.nix { inherit aws; };
  };
}
