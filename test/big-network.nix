{ lib, ... }: with lib;
let
  region = "ap-southeast-1";
  zone = "ap-southeast-1b";
  accessKeyId = "default";
  keyPair = "keypair";

  instance = instanceType: { infra, lib, ... }: with lib; {
    ec2 = {
      inherit region zone accessKeyId;
      inherit instanceType;
      inherit keyPair;
#      securityGroups = [ infra.ec2SecurityGroups.default ];
#      subnet = infra.subnets.default;
      instanceProfileARN = "arn:aws:iam::555555555555:instance-profile/yay";
    };
  };

in
{
  infra.vpc.default = {
    inherit region accessKeyId;
    cidrBlock = "10.15.0.0/16";
  };

  infra.subnets.default = { infra, ... }: {
    inherit region zone accessKeyId;
    cidrBlock = "10.15.0.0/24";
    vpc = builtins.trace (attrNames infra) infra.vpc.default;
#    vpc = builtins.trace (attrNames infra.vpc.default) "000000000=========>";
#    vpc = "12830128301273091283901283";
  };

  infra.ebsVolumes.mysql-master = {
    inherit region zone accessKeyId;
    name = "hello-mysql-master";
    size = 200;
  };

  infra.ec2SecurityGroups.default = { infra, ... }: {
    inherit region accessKeyId;
#    vpc = infra.vpc.default;
    vpc = "undef";
    rules = [
      { protocol = "icmp";              sourceIp = "0.0.0.0/0"; }
      { fromPort = 22;  toPort = 22;    sourceIp = "0.0.0.0/0"; }
      { fromPort = 443; toPort = 443;   sourceIp = "0.0.0.0/0"; }
#      { fromPort = 0;   toPort = 65535; sourceIp = infra.subnets.default.cidrBlock; }
    ];
  };

  infra.instances.web1 = instance "m3.large";
  infra.instances.web2 = instance "m3.large";
  infra.instances.db-master = instance "r3.xlarge";
  infra.instances.solr = instance "m3.large";

  infra.elbs.web = { infra, lib, ... }: with lib; {
    inherit region accessKeyId;

    subnets = []; securityGroups = []; instances = [];
#    subnets = [ infra.subnets.default ];
#    securityGroups = [ infra.ec2SecurityGroups.default ];
#    instances = with infra.instances; [ web1 web2 ];

    listeners =
      let
        http = {
          lbPort = 80;
          lbProtocol = "http";
          instancePort = 80;
          instanceProtocol = "http";
        };
        https = {
          lbPort = 443;
          lbProtocol = "https";
          instancePort = 80;
          instanceProtocol = "http";
          sslCertificateId = "123";
        };
      in [ http https ];
  };
}
