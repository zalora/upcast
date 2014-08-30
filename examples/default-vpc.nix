{ lib ? (import <nixpkgs> {}).lib
, accessKeyId ? "default"
, region
}:

{
  resources.vpc."${region}-default" = {
    inherit region accessKeyId;
    cidrBlock = "10.15.0.0/16";
  };

  resources.subnets."${region}-default-a" = { resources, ... }: {
    inherit region accessKeyId;
    vpc = resources.vpc."${region}-default";
    zone = "${region}a";
    cidrBlock = "10.15.0.0/18";
  };

  resources.subnets."${region}-default-b" = { resources, ... }: {
    inherit region accessKeyId;
    vpc = resources.vpc."${region}-default";
    zone = "${region}b";
    cidrBlock = "10.15.64.0/18";
  };

  #resources.subnets."${region}-default-c" = { resources, ... }: {
  #  inherit region accessKeyId;
  #  vpc = resources.vpc."${region}-default";
  #  zone = "${region}b";
  #  cidrBlock = "10.15.128.0/18";
  #};

  resources.ec2SecurityGroups."${region}-default" = { resources, ... }: {
    inherit region accessKeyId;
    vpc = resources.vpc."${region}-default";
    rules = import ./sg-ssh-https-offices.nix { inherit lib; };
  };
}
