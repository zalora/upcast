{

  machines = {
    pluralDisplayName = "machines";

    baseModules = import <nixpkgs/nixos/modules/module-list.nix> ++ [ ./options.nix ];
  };

  vpc = {
    pluralDisplayName = "EC2 VPCs";

    baseModules = [ ./ec2-vpc.nix ./resource.nix ];
  };

  subnets = {
    pluralDisplayName = "EC2 VPC Subnets";

    baseModules = [ ./ec2-subnet.nix ./resource.nix ];
  };

  ec2KeyPairs = {
    pluralDisplayName = "EC2 keypairs";

    baseModules = [ ./ec2-keypair.nix ./resource.nix ];
  };

  ec2SecurityGroups = {
    pluralDisplayName = "EC2 security groups";

    baseModules = [ ./ec2-security-group.nix ./resource.nix ];
  };

  ebsVolumes = {
    pluralDisplayName = "EBS volumes";

    baseModules = [ ./ebs-volume.nix ./resource.nix ];
  };

  elbs = {
    pluralDisplayName = "Elastic load balancers";

    baseModules = [ ./elb.nix ./resource.nix ];
  };
}
