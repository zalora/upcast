#SANDBOX_STUFF= -no-user-package-db -package-db "$(shell awk '/^package-db:/ {print $$2}' cabal.sandbox.config)"
SANDBOX_STUFF=

DIST= .stack-work/dist/x86_64-osx/Cabal-1.18.1.5/
#DIST= dist
GHC= stack exec -- ghc

ghci:
	env NIX_UPCAST=$(PWD)/nix $(GHC) --interactive -fbuilding-cabal-package -O0 -outputdir $(DIST)/build -odir $(DIST)/build -hidir $(DIST)/build -stubdir $(DIST)/build -i -i$(DIST)/build -isrc -i$(DIST)/build/autogen -I$(DIST)/build/autogen -I$(DIST)/build -XHaskell2010 -XOverloadedStrings $(SANDBOX_STUFF) src/upcast.hs

test-tree:
	NIX_UPCAST=nix upcast infra-tree test/big-network.nix | jq -M -r .

infra-nix: src/Upcast/Infra/NixTypes.hs

src/Upcast/Infra/NixTypes.hs: nix/inspect-types.nix
	nix-instantiate -I upcast=nix --eval --strict --json --show-trace $< | jq -r . > $@

.PHONY: ghci test-tree inspect-types
