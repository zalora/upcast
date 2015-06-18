{ lib, ... }: with lib;
let
  region = "ap-southeast-1";
  zone = "ap-southeast-1b";
  accessKeyId = "default";
  keyPair = "keypair";

  instance = instanceType: { infra, lib, ... }: with lib; {
    inherit region zone accessKeyId;
    inherit instanceType;
    inherit keyPair;
    securityGroups = [ infra.ec2-sg.default ];
    subnet = infra.ec2-subnet.default;
    instanceProfileARN = "arn:aws:iam::555555555555:instance-profile/yay";
  };
in
{
  # infra.ec2-keypair.global-keypair = {
  #  inherit region accessKeyId;
  #  name = "global-keypair";
  #  privateKeyFile = "${<upcast/../test/big-network.nix>}";
  # };

  infra.ec2-vpc.default = {
    inherit region accessKeyId;
    cidrBlock = "10.15.0.0/16";
  };

  infra.ec2-subnet.default = { infra, ... }: {
    inherit region zone accessKeyId;
    cidrBlock = "10.15.0.0/24";
    vpc = infra.ec2-vpc.default;
  };

  infra.ebs.mysql-master = {
    inherit region zone accessKeyId;
    name = "hello-mysql-master";
    size = 200;
  };

  infra.ec2-sg.default = { infra, ... }: {
    inherit region accessKeyId;
    vpc = infra.ec2-vpc.default;
    rules = [
      { protocol = "icmp";              sourceIp = "0.0.0.0/0"; }
      { fromPort = 22;  toPort = 22;    sourceIp = "0.0.0.0/0"; }
      { fromPort = 443; toPort = 443;   sourceIp = "0.0.0.0/0"; }
      { fromPort = 0;   toPort = 65535; sourceIp = infra.ec2-subnet.default.cidrBlock; }
    ];
  };

  infra.ec2-instance.web1 = instance "m3.large";
  infra.ec2-instance.web2 = instance "m3.large";
  infra.ec2-instance.db-master = instance "r3.xlarge";
  infra.ec2-instance.solr = instance "m3.large";

  infra.elb.web = { infra, lib, ... }: with lib; {
    inherit region accessKeyId;

    subnets = [ infra.ec2-subnet.default ];
    securityGroups = [ infra.ec2-sg.default ];
    instances = with infra.ec2-instance; [ web1 web2 ];

    healthCheck = {
      timeout = 5;
      interval = 30;
      healthyThreshold = 2;
      unhealthyThreshold = 10;
      target.tcp = 80;
    };

    listeners =
      let
        http = {
          lbPort = 80;
          lbProtocol = "http";
          instancePort = 80;
          instanceProtocol = "http";
          stickiness.lb = null;
        };
        https = {
          lbPort = 443;
          lbProtocol = "https";
          instancePort = 80;
          instanceProtocol = "http";
          sslCertificateId = "123";
        };
      in [ http https ];

    route53Aliases."elb.example.com".zoneId = "ZOZONEZONEZONE";
  };
}
