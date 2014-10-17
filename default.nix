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

  # working git must be in $PATH.
  fetchgit =
    {url, rev ? "origin/master", sha256 ? "whatever", shallow ? false}:
      shell (baseNameOf (toString url)) ''
        git clone ${if shallow then "--depth 1 -b ${rev}" else ""} --recursive ${url} $out
        cd $out
        ${if shallow then "" else "git checkout ${rev}"}
      '';

   nixpkgs = fetchgit {
    url = "git://github.com/zalora/nixpkgs.git";
    rev = "upcast";
    shallow = true;
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
