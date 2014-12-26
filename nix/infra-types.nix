{
  instances = {
    pluralDisplayName = "EC2 Instances";

    baseModules = [ ./ec2.nix ./infra.nix ];
  };

  vpc = {
    pluralDisplayName = "EC2 VPCs";

    baseModules = [ ./ec2-vpc.nix ./infra.nix ];
  };

  subnets = {
    pluralDisplayName = "EC2 VPC Subnets";

    baseModules = [ ./ec2-subnet.nix ./infra.nix ];
  };

  ec2KeyPairs = {
    pluralDisplayName = "EC2 keypairs";

    baseModules = [ ./ec2-keypair.nix ./infra.nix ];
  };

  ec2SecurityGroups = {
    pluralDisplayName = "EC2 security groups";

    baseModules = [ ./ec2-security-group.nix ./infra.nix ];
  };

  ebsVolumes = {
    pluralDisplayName = "EBS volumes";

    baseModules = [ ./ebs-volume.nix ./infra.nix ];
  };

  elbs = {
    pluralDisplayName = "Elastic load balancers";

    baseModules = [ ./elb.nix ./infra.nix ];
  };
}
