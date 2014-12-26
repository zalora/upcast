{ config, lib, ... }:

with lib;

let
  cfg = config.ec2;

  inherit (import ./option-types.nix { inherit lib; }) union infra;

  ec2DiskOptions = { config, ... }: {
    options = {
      disk = mkOption {
        default = "";
        example = "vol-d04895b8";
        type = union types.str (infra "ebs-volume");
        apply = x: if builtins.isString x then x else "res-" + x._name;
        description = ''
          EC2 identifier of the disk to be mounted.  This can be an
          ephemeral disk (e.g. <literal>ephemeral0</literal>), a
          snapshot ID (e.g. <literal>snap-1cbda474</literal>) or a
          volume ID (e.g. <literal>vol-d04895b8</literal>).  Leave
          empty to create an EBS volume automatically.  It can also be
          an EBS infra (e.g. <literal>infras.ebsVolumes.big-disk</literal>).
          '';
      };

      blockDeviceMappingName = mkOption {
        default = "";
        type = types.str;
        example = "/dev/sdb";
        description = ''
          The name of the block device that's accepted by AWS' RunInstances.
          Must properly map to <literal>fileSystems.device</literal> option.
          Leave blank for defaults.
        '';
      };

      fsType = mkOption {
        default = "ext4"; # FIXME: this default doesn't work
        type = types.str;
        description = ''
          Filesystem type for automatically created EBS volumes.
        '';
      };
    };
  };

in
{
  options = {

    ec2.accessKeyId = mkOption {
      default = "";
      example = "AKIAIEMEJZVMPOHZWKZQ";
      type = types.str;
      description = ''
        This option is (yet) ignored by Upcast.
      '';
    };

    ec2.region = mkOption {
      default = "";
      example = "us-east-1";
      type = types.str;
      description = ''
        Amazon EC2 region in which the instance is to be deployed.
        This option only applies when using EC2.  It implicitly sets
        <option>ec2.ami</option>.
      '';
    };

    ec2.zone = mkOption {
      default = "";
      example = "us-east-1c";
      type = types.str;
      description = ''
        The EC2 availability zone in which the instance should be
        created.  If not specified, a zone is selected automatically.
      '';
    };

    ec2.ami = mkOption {
      example = "ami-ecb49e98";
      type = types.str;
      description = ''
        EC2 identifier of the AMI disk image used in the virtual
        machine.
      '';
    };

    ec2.ebsBoot = mkOption {
      default = true;
      type = types.bool;
      description = ''
        Whether you want to boot from an EBS-backed AMI.  Only
        EBS-backed instances can be stopped and restarted, and attach
        other EBS volumes at boot time.  This option determines the
        selection of the default AMI; if you explicitly specify
        <option>ec2.ami</option>, it has no effect.
      '';
    };

    ec2.instanceType = mkOption {
      default = "m1.small";
      example = "m1.large";
      type = types.str;
      description = ''
        EC2 instance type.  See <link
        xlink:href='http://aws.amazon.com/ec2/instance-types/'/> for a
        list of valid Amazon EC2 instance types.
      '';
    };

    ec2.instanceProfileARN = mkOption {
      default = "";
      example = "arn:aws:iam::123456789012:instance-profile/S3-Permissions";
      type = types.str;
      description = ''
        The ARN of the IAM Instance Profile (IIP) to associate with
        the instances.
      '';
    };

    ec2.keyPair = mkOption {
      example = "my-keypair";
      type = union types.str (infra "ec2-keypair");
      apply = x: if builtins.isString x then x else x.name;
      description = ''
        Name of the SSH key pair to be used to communicate securely
        with the instance.  Key pairs can be created using the
        <command>ec2-add-keypair</command> command.
      '';
    };

    ec2.securityGroups = mkOption {
      default = [ "default" ];
      example = [ "my-group" "my-other-group" ];
      type = types.listOf (union types.str (infra "ec2-security-group"));
      apply = map (x: if builtins.isString x then x else x._name);
      description = ''
        Security groups for the instance.  These determine the
        firewall rules applied to the instance.
      '';
    };

    ec2.blockDeviceMapping = mkOption {
      default = { };
      example = { "/dev/xvdb".disk = "ephemeral0"; "/dev/xvdg".disk = "vol-d04895b8"; };
      type = types.attrsOf types.optionSet;
      options = ec2DiskOptions;
      description = ''
        Block device mapping.  <filename>/dev/xvd[a-e]</filename> must be ephemeral devices.
      '';
    };

    ec2.ebsOptimized = mkOption {
      default = false;
      type = types.bool;
      description = ''
        Whether the EC2 instance should be created as an EBS Optimized instance.
        (Requires you to pick a proper instanceType)
      '';
    };

    ec2.subnet = mkOption {
      type = types.nullOr (union types.str (infra "ec2-subnet"));
      default = null;
      apply = x: if x == null then null else if builtins.isString x then x else x._name;
      description = ''
        EC2 VPC subnet id
      '';
    };

  };

  config = {
    ec2.ami = mkDefault (
      let
        isEc2Hvm =
            cfg.instanceType == "cc1.4xlarge"
         || cfg.instanceType == "cc2.8xlarge"
         || cfg.instanceType == "hs1.8xlarge"
         || cfg.instanceType == "cr1.8xlarge"
         || builtins.substring 0 2 cfg.instanceType == "i2"
         || builtins.substring 0 2 cfg.instanceType == "c3"
         || builtins.substring 0 2 cfg.instanceType == "r3"
         || builtins.substring 0 2 cfg.instanceType == "m3"
         || builtins.substring 0 2 cfg.instanceType == "t2";

        type = if isEc2Hvm then "hvm" else if cfg.ebsBoot then "ebs" else "s3";
        amis = import ./ec2-amis.nix;
        amis' = amis."14.04"; # default to 14.04 images
      in
        with builtins;
        if hasAttr cfg.region amis' then
          let r = amis'."${cfg.region}";
          in if hasAttr type r then r."${type}" else ""
        else
          ""
      );
  };
}
