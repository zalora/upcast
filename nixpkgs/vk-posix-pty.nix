{ cabal }:

cabal.mkDerivation (self: {
  pname = "vk-posix-pty";
  version = "0.2.0.2";
  sha256 = "0dp9kpn1qzfqykmyq20sg0snyk8zrqa2g8c5xp9rfp0zfgmh37dh";
  isLibrary = true;
  isExecutable = true;
  meta = {
    homepage = "https://github.com/proger/posix-pty/tree/fork";
    description = "Pseudo terminal interaction with subprocesses";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
