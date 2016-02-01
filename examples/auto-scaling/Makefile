builder ?= localhost

ami.txt: spec.nix
	upcast build -t $(builder) -A ami spec.nix -- \
		-I nixpkgs=https://github.com/NixOS/nixpkgs/archive/fe8fd63e39301a8b0e5a5ef81d90544126f44962.tar.gz \
		-I microgram=https://github.com/zalora/microgram/archive/505ec02a991620ec349ad68b0f6615b371679f88.tar.gz \
	| xargs -0t -n1 ssh -A $(builder) | tr -d '\n' | tee ami.txt

provision: ami.txt
	upcast infra -v spec.nix
.PHONY: provision
