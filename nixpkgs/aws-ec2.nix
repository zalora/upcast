{ cabal, aeson, aws, base16Bytestring, base64Bytestring
, blazeBuilder, byteable, conduitExtra, cryptohash, httpConduit
, httpTypes, mtl, optparseApplicative, resourcet, scientific, text
, time, unorderedContainers, vector, xmlConduit
}:

cabal.mkDerivation (self: {
  pname = "aws-ec2";
  version = "0.2";
  sha256 = "0cixql62n0gn49bv0k9q9sdj3qxan1xgzcvzl6754z8qq3q0jg0k";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson aws base16Bytestring base64Bytestring blazeBuilder byteable
    conduitExtra cryptohash httpConduit httpTypes mtl
    optparseApplicative resourcet scientific text time
    unorderedContainers vector xmlConduit
  ];
  meta = {
    homepage = "https://github.com/zalora/aws-ec2";
    description = "AWS EC2/VPC, ELB and CloudWatch client library for Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
