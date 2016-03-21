ghci:
	stack ghci

test-tree:
	NIX_UPCAST=nix upcast infra-tree test/big-network.nix

infra-nix: lib/Infracast/NixTypes.hs

lib/Infracast/NixTypes.hs: nix/inspect-types.nix
	nix-instantiate -I upcast=nix --eval --strict --json --show-trace $< | jq -r . > $@

.PHONY: ghci test-tree inspect-types
