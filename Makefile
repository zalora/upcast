#SANDBOX_STUFF= -no-user-package-db -package-db "$(shell awk '/^package-db:/ {print $$2}' cabal.sandbox.config)"
SANDBOX_STUFF=

ghci:
	env NIX_UPCAST=$(PWD)/nix ghc --interactive -fbuilding-cabal-package -O0 -outputdir dist/build -odir dist/build -hidir dist/build -stubdir dist/build -i -idist/build -isrc -idist/build/autogen -Idist/build/autogen -Idist/build -XHaskell2010 -XOverloadedStrings -XTemplateHaskell $(SANDBOX_STUFF) src/upcast.hs

test-tree:
	NIX_UPCAST=nix upcast infra-tree test/big-network.nix | jq -M -r .

inspect-types:
	nix-instantiate -I upcast=nix --eval --strict --json --show-trace nix/inspect-types.nix

.PHONY: ghci test-tree inspect-types
