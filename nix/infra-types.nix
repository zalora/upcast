{
  ec2-instance = {
    pluralDisplayName = "EC2 Instances";

    baseModules = [ ./aws/ec2-instance.nix ./infra-base.nix ];
  };

  ec2-vpc = {
    pluralDisplayName = "EC2 VPCs";

    baseModules = [ ./aws/ec2-vpc.nix ./infra-base.nix ];
  };

  ec2-subnet = {
    pluralDisplayName = "EC2 VPC Subnets";

    baseModules = [ ./aws/ec2-subnet.nix ./infra-base.nix ];
  };

  ec2-keypair = {
    pluralDisplayName = "EC2 keypairs";

    baseModules = [ ./aws/ec2-keypair.nix ./infra-base.nix ];
  };

  ec2-sg = {
    pluralDisplayName = "EC2 security groups";

    baseModules = [ ./aws/ec2-sg.nix ./infra-base.nix ];
  };

  ebs = {
    pluralDisplayName = "EBS volumes";

    baseModules = [ ./aws/ebs-volume.nix ./infra-base.nix ];
  };

  elb = {
    pluralDisplayName = "Elastic load balancers";

    baseModules = [ ./aws/elb.nix ./infra-base.nix ];
  };
}
