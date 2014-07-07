## upcast

(extremely WIP) Next generation cloud deployment tool, replacement for NixOps.

### Motivation

![motivation](http://i.imgur.com/HY2Gtk5.png)

### Goals

- simplicity, extensibility
- minimum dependency of network latency of the client
- shared state stored as nix expressions next to machines expressions
- first-class Nix support,basic compatbility with nixops to automatically lift existing deployments
- first-class AWS support (including AWS features nixops doesn't have), as a side-effect, deliver a command-line ec2 toolchain
- support for running day-to-day operations on deployed resources, services and machines

### Non-goals

- cloning nixops
- supporting development environments with virtualbox

### Roadmap

- implement nixops deploy with VPC and ELB support (needed for eris)
  - proxy (reuse) missing vital features to nixops if possible
- document
- ...
- PROFIT!

### Running

Initially developed with GHC 7.8.2 and Cabal 1.20 localhost-style. YMMV.

```
$ cabal configure -O2
$ cabal build
$ dist/build/ec2/ec2
$ dist/build/upcast/upcast
```

