{ config, name, lib, ... }:

let
  common = import ./common.nix { inherit lib; };
  inherit (common) sum;
  inherit (lib) types mkOption mkIf mkDefault;
in
{
  options = {
    name = mkOption {
      example = "My Big Fat Disk";
      default = name;
      type = types.str;
      description = "Description of the EBS volume.  This is the <literal>Name</literal> tag of the disk.";
    };

    inherit (common) accessKeyId region zone;

    size = mkOption {
      example = 100;
      type = types.int;
      description = ''
        Volume size (in gigabytes).  This may be left unset if you are
        creating the volume from a snapshot, in which case the size of
        the volume will be equal to the size of the snapshot.
        However, you can set a size larger than the snapshot, allowing
        the volume to be larger than the snapshot from which it is
        created.
      '';
    };

    volumeType = mkOption {
      type = sum {
        standard = null;
        gp2 = null;
        iop = types.int;
      };
      default = { standard = null; };
      description = ''
        EBS volume type. Defaults to standard magnetic.
      '';
    };

    snapshot = mkOption {
      default = "";
      example = "snap-1cbda474";
      type = types.str;
      description = ''
        The snapshot ID from which this volume will be created.  If
        not specified, an empty volume is created.  Changing the
        snapshot ID has no effect if the volume already exists.
      '';
    };

  };

  config = {
    _type = "ebs-volume";
    size = mkIf (config.snapshot != "") (mkDefault 0);
  };

}
