ghci:
	ghc --interactive -fbuilding-cabal-package -O0 -outputdir dist/build -odir dist/build -hidir dist/build -stubdir dist/build -i -idist/build -isrc -idist/build/autogen -Idist/build/autogen -Idist/build src/upcast.hs

.PHONY: ghci
