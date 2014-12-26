### nix tests

```
env NIX_PATH= nix-instantiate -I upcast=$HOME/src/upcast/nix --eval --strict --arg expr ./big-network.nix '<upcast/eval-infra.nix>' --show-trace --json | jq -r .
```
