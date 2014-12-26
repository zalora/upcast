#SANDBOX_STUFF= -no-user-package-db -package-db "$(shell awk '/^package-db:/ {print $$2}' cabal.sandbox.config)"
SANDBOX_STUFF=

ghci:
	env NIX_UPCAST=$(PWD)/nix ghc --interactive -fbuilding-cabal-package -O0 -outputdir dist/build -odir dist/build -hidir dist/build -stubdir dist/build -i -idist/build -isrc -idist/build/autogen -Idist/build/autogen -Idist/build -XHaskell2010 -XOverloadedStrings -XTemplateHaskell $(SANDBOX_STUFF) src/upcast.hs

.PHONY: ghci
