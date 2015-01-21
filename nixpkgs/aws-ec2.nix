{ cabal, aeson, aws, base16Bytestring, base64Bytestring
, blazeBuilder, byteable, conduitExtra, cryptohash, httpConduit
, httpTypes, mtl, optparseApplicative, resourcet, scientific, text
, time, unorderedContainers, vector, xmlConduit, yaml
}:

cabal.mkDerivation (self: {
  pname = "aws-ec2";
  version = "0.3.1";
  sha256 = "0na1rszj6izsyi76fjb90phsp2af79fpnykqy3mhssdsjy46bq64";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson aws base16Bytestring base64Bytestring blazeBuilder byteable
    conduitExtra cryptohash httpConduit httpTypes mtl
    optparseApplicative resourcet scientific text time
    unorderedContainers vector xmlConduit yaml
  ];
  meta = {
    homepage = "https://github.com/zalora/aws-ec2";
    description = "AWS EC2/VPC, ELB and CloudWatch client library for Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
