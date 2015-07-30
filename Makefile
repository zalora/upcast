ghci:
	stack ghci

test-tree:
	NIX_UPCAST=nix upcast infra-tree test/big-network.nix | jq -M -r .

infra-nix: src/Upcast/Infra/NixTypes.hs

src/Upcast/Infra/NixTypes.hs: nix/inspect-types.nix
	nix-instantiate -I upcast=nix --eval --strict --json --show-trace $< | jq -r . > $@

.PHONY: ghci test-tree inspect-types
