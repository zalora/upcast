let
  unsafeDerivation = args: derivation ({
    PATH = builtins.getEnv "PATH";
    system = builtins.currentSystem;
    builder = ./bootstrap/proxy-bash.sh;
    preferLocalBuild = true;
    __noChroot = true;
  } // args);

  shell-fn = name: command: unsafeDerivation {
    inherit name;
    args = ["-c" command];
  };

  # working git must be in $PATH.
  # increase (change) `clock' to trigger updates
  shallow-fetchgit =
    {url, branch ? "master", clock ? 1}:
      shell-fn "${baseNameOf (toString url)}-${toString clock}" ''
        git clone --depth 1 -b ${branch} --recursive ${url} $out
        cd $out
      '';

  nixpkgs = shallow-fetchgit {
    url = "git://github.com/zalora/nixpkgs.git";
    branch = "upcast";
    clock = 2;
  };
in
{ system ? builtins.currentSystem
, pkgs ? import nixpkgs { inherit system; }
, shell ? (if builtins.currentSystem == "x86_64-darwin"
           then shell-fn
           else k: v: pkgs.runCommand k { preferLocalBuild = true; } v)
, name ? "upcast"
, src ? builtins.filterSource (path: type: let base = baseNameOf path; in
    type != "unknown" &&
    base != ".git" && base != "result" && base != "dist" && base != ".cabal-sandbox"
    ) ./.
, haskellPackages ? pkgs.haskellPackages_ghc783
}:

let
  upcast =
    { cabal, aeson, aesonPretty, async, attoparsec, aws, awsEc2
    , base64Bytestring, conduit, conduitExtra, filepath, free
    , haskellSrcMeta, httpConduit, httpTypes, liftedBase, mtl
    , optparseApplicative, prettyShow, random, resourcet, scientific
    , text, time, unorderedContainers, vector, vkAwsRoute53, vkPosixPty
    }:

    cabal.mkDerivation (self: {
      pname = "upcast";
      inherit src;
      version = "0.1.0.0";
      isLibrary = false;
      isExecutable = true;
      jailbreak = true;
      buildDepends = [
        aeson aesonPretty async attoparsec aws base64Bytestring
        conduit conduitExtra filepath free haskellSrcMeta httpConduit
        httpTypes liftedBase mtl optparseApplicative prettyShow random
        resourcet scientific text time unorderedContainers vector

        awsEc2 vkAwsRoute53 vkPosixPty
      ];
      meta = {
        license = self.stdenv.lib.licenses.mit;
        platforms = self.ghc.meta.platforms;
      };
    });
in

haskellPackages.callPackage upcast {
  vkAwsRoute53 = haskellPackages.callPackage ./nixpkgs/vk-aws-route53.nix {};
  awsEc2 = haskellPackages.callPackage ./nixpkgs/aws-ec2.nix {};
  vkPosixPty = haskellPackages.callPackage ./nixpkgs/vk-posix-pty.nix {};
}
