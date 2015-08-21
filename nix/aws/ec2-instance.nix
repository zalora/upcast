{ config, lib, ... }: with lib;

let
  common = import ./common.nix { inherit lib; };
  inherit (common) infra;

  ec2DiskOptions = { config, ... }: {
    options = {
      disk = mkOption {
        default = "";
        example = "vol-d04895b8";
        type = infra "ebs";
        description = ''
          EC2 identifier of the disk to be mounted.  This can be an
          ephemeral disk (e.g. <literal>ephemeral0</literal>) or a
          volume ID (e.g. <literal>vol-d04895b8</literal>).
          It can be an EBS infra
          (e.g. <literal>infra.ebs.big-disk</literal>).
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
    inherit (import ./common.nix { inherit lib; }) accessKeyId region zone;

    subnet = common.nullOr common.subnet;

    ami = mkOption {
      example = "ami-ecb49e98";
      type = types.str;
      description = ''
        EC2 identifier of the AMI disk image used in the virtual
        machine.
        NixOps default AMIs: https://github.com/NixOS/nixops/blob/master/nix/ec2-amis.nix
      '';
    };

    ebsBoot = mkOption {
      default = true;
      type = types.bool;
      description = ''
        Whether you want to boot from an EBS-backed AMI.  Only
        EBS-backed instances can be stopped and restarted, and attach
        other EBS volumes at boot time.  This option determines the
        selection of the default AMI; if you explicitly specify
        <option>ami</option>, it has no effect.
      '';
    };

    instanceType = mkOption {
      default = "m1.small";
      example = "m1.large";
      type = types.str;
      description = ''
        EC2 instance type.  See <link
        xlink:href='http://aws.amazon.com/ec2/instance-types/'/> for a
        list of valid Amazon EC2 instance types.
      '';
    };

    instanceProfileARN = mkOption {
      default = null;
      example = "arn:aws:iam::123456789012:instance-profile/S3-Permissions";
      type = types.nullOr types.str;
      description = ''
        The ARN of the IAM Instance Profile (IIP) to associate with
        the instances.
      '';
    };

    keyPair = mkOption {
      example = "my-keypair";
      type = infra "ec2-keypair";
      description = ''
        Name of the SSH key pair to be used to communicate securely
        with the instance.  Key pairs can be created using the
        <command>ec2-add-keypair</command> command.
      '';
    };

    securityGroups = mkOption {
      default = [ "default" ];
      example = [ "my-group" "my-other-group" ];
      type = types.listOf (infra "ec2-sg");
      description = ''
        Security groups for the instance.  These determine the
        firewall rules applied to the instance.
      '';
    };

    blockDeviceMapping = mkOption {
      default = { };
      example = { "/dev/xvdb".disk = "ephemeral0"; "/dev/xvdg".disk = "vol-d04895b8"; };
      type = types.attrsOf (types.submodule ec2DiskOptions);
      description = ''
        Block device mapping.  <filename>/dev/xvd[a-e]</filename> must be ephemeral devices.
      '';
    };

    ebsOptimized = mkOption {
      default = false;
      type = types.bool;
      description = ''
        Whether the EC2 instance should be created as an EBS Optimized instance.
        (Requires you to pick a proper instanceType)
      '';
    };

    userData = mkOption {
      default = {};
      type = types.attrsOf types.path;
      example = { host-aes-key = "./secrets/aes-key"; };
      description = ''
        Attribute set containing mappings to files that will be passed in as user data.
      '';
    };
  };

  config._type = "ec2-instance";
}
