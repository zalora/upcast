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

let
  aws = haskellPackages.callPackage ./nixpkgs/aws.nix {};
in

haskellPackages.buildLocalCabalWithArgs {
  inherit src name;
  args = {
    inherit aws;
    vkAwsRoute53 = haskellPackages.callPackage ./nixpkgs/vk-aws-route53.nix { inherit aws; };
    awsEc2 = haskellPackages.callPackage ./nixpkgs/aws-ec2.nix { inherit aws; };
  };
}
