### ireland-network.nix

Creates a VPC, two subnets and a security group in `eu-west-1`.
(Doesn't start any instances.)

Try it:

```
% upcast run ireland-network.nix
% less ireland-network.store
```

### webapp-internal-elb.nix

Creates a two-node deployment (based on a single AMI) in a single VPC subnet and a single AZ balanced by an internal ELB.

```
% vim webapp-internal-elb.nix   # you need to tweak vpc/keypair/ami parameters before trying
% upcast run webapp-internal-elb.nix
```
