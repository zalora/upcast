{ config, pkgs, lib ? pkgs.lib, ... }:

with lib;

let

  cfg = config.deployment;

in

{

  imports = [
    ./ec2.nix
    ./keys.nix
  ];

  options = {

    deployment.targetEnv = mkOption {
      default = "none";
      example = "ec2";
      type = types.str;
      description = ''
        This option specifies the type of the environment in which the
        machine is to be deployed by NixOps.  Currently, it can have
        the following values. <literal>"none"</literal> means
        deploying to a pre-existing physical or virtual NixOS machine,
        reachable via SSH under the hostname or IP address specified
        in <option>deployment.targetHost</option>.
        <literal>"ec2"</literal> means that a virtual machine should
        be instantiated in an Amazon EC2-compatible cloud environment
        (see <option>deployment.ec2.*</option>).
        <literal>"virtualbox"</literal> causes a VirtualBox VM to be
        created on your machine.  (This requires VirtualBox to be
        configured on your system.)  <!-- <literal>"adhoc-cloud"</literal>
        means that a virtual machine should be instantiated by
        executing certain commands via SSH on a cloud controller
        machine (see <option>deployment.adhoc.*</option>).  This is
        primarily useful for debugging NixOps. -->
      '';
    };

    deployment.targetHost = mkOption {
      type = types.str;
      description = ''
        This option specifies the hostname or IP address to be used by
        NixOps to execute remote deployment operations.
      '';
    };

  };


  config = {
    deployment.targetHost = mkDefault config.networking.hostName;
  };

}
