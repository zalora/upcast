### nix tests

```
nix-instantiate -I nixpkgs=$HOME/src/nix-upstream/nixpkgs -I upcast=$HOME/src/upcast/nix --eval --strict --arg expr './big-network.nix' '<upcast/eval-upstream.nix>' --show-trace --json | jq -r .

nix-instantiate -I upcast=$HOME/src/upcast/nix --eval --strict --arg exprs './big-network.nix' -A infra '<upcast/eval-deployment.nix>' --json | jq -r .
```
