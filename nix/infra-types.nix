{
  instances = {
    pluralDisplayName = "EC2 Instances";

    baseModules = [ ./aws/ec2-instance.nix ./infra-base.nix ];
  };

  vpc = {
    pluralDisplayName = "EC2 VPCs";

    baseModules = [ ./aws/ec2-vpc.nix ./infra-base.nix ];
  };

  subnets = {
    pluralDisplayName = "EC2 VPC Subnets";

    baseModules = [ ./aws/ec2-subnet.nix ./infra-base.nix ];
  };

  ec2KeyPairs = {
    pluralDisplayName = "EC2 keypairs";

    baseModules = [ ./aws/ec2-keypair.nix ./infra-base.nix ];
  };

  ec2SecurityGroups = {
    pluralDisplayName = "EC2 security groups";

    baseModules = [ ./aws/ec2-security-group.nix ./infra-base.nix ];
  };

  ebsVolumes = {
    pluralDisplayName = "EBS volumes";

    baseModules = [ ./aws/ebs-volume.nix ./infra-base.nix ];
  };

  elbs = {
    pluralDisplayName = "Elastic load balancers";

    baseModules = [ ./aws/elb.nix ./infra-base.nix ];
  };
}
