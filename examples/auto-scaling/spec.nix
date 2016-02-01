{ ami = (import <microgram/nixos> {
    configuration = { config, lib, ... }: {
      imports = [
        <microgram/nixos/ec2>
        <nixpkgs/nixos/modules/services/web-servers/nginx>
      ];
      config.services.nginx.enable = true;
      config.services.nginx.httpConfig = ''
        server {
          listen 80 default_server;
          return 200 "$server_addr";
        }
      '';
    };
  }).config.system.build.aminator;

  infra = { infra, ... }: let
    region = "ap-northeast-1";
    zone = "${region}a"; # xxx: distribute over [a,b]
    subnet = zone: cidrBlock: {
     inherit region zone cidrBlock;
     vpc = infra.ec2-vpc.default;
    };
  in {
    ec2-vpc.default = {
      inherit region;
      cidrBlock = "10.15.1.0/24";
    };
    ec2-subnet.default = {
      inherit region;
      zone = "${region}a";
      vpc = infra.ec2-vpc.default;
      cidrBlock = "10.15.1.0/24";
    };
    ec2-sg.default = {
      inherit region;
      vpc = infra.ec2-vpc.default;
      rules = [
        { protocol = "icmp"; fromPort = -1;  toPort = -1; sourceIp = "0.0.0.0/0"; }
        { protocol = "tcp";  fromPort = 22;  toPort = 22; sourceIp = "0.0.0.0/0"; }
        { protocol = "tcp";  fromPort = 80;  toPort = 80; sourceIp = "0.0.0.0/0"; }
      ];
    };
    elb.default = {
      inherit region;
      subnets = [infra.ec2-subnet.default];
      securityGroups = [infra.ec2-sg.default];
      listeners = [{
       lbPort = 80; lbProtocol = "http";
       instancePort = 80; instanceProtocol = "http";
      }];
    };
    launch-configuration.default = {
      inherit (infra.ec2-subnet.default) zone;
      inherit region;
      instanceType = "m3.medium";
      ami = builtins.readFile ./ami.txt;
      securityGroups = [infra.ec2-sg.default];
    };
    autoscaling-group.default = {
      inherit (infra.elb.default) subnets;
      inherit (infra.ec2-subnet.default) zone;
      inherit region;
      healthcheckType = "ELB";
      launchConfiguration = infra.launch-configuration.default;
      minSize = 1; maxSize = 2;
      loadBalancers = [infra.elb.default];
    };
  };
}
