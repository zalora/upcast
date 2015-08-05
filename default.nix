{ mkDerivation, aeson, aeson-pretty, amazonka, amazonka-autoscaling
, amazonka-core, amazonka-ec2, amazonka-elb, amazonka-route53
, async, attoparsec, base, base64-bytestring, bytestring, conduit
, conduit-extra, containers, directory, exceptions, filepath, lens
, lifted-base, mtl, natural-sort, optparse-applicative, pretty-show
, process, random, resourcet, scientific, semigroups, stdenv
, tagged, text, time, unix, unordered-containers, vector
, vk-posix-pty, xml-conduit
}:
mkDerivation {
  pname = "upcast";
  version = "0.1.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson aeson-pretty amazonka amazonka-autoscaling amazonka-core
    amazonka-ec2 amazonka-elb amazonka-route53 async attoparsec base
    base64-bytestring bytestring conduit conduit-extra containers
    directory exceptions filepath lens lifted-base mtl natural-sort
    optparse-applicative pretty-show process random resourcet
    scientific semigroups tagged text time unix unordered-containers
    vector vk-posix-pty xml-conduit
  ];
  homepage = "https://github.com/zalora/upcast#readme";
  description = "Nix-based Linux deployment platform tools";
  license = stdenv.lib.licenses.mit;
}
