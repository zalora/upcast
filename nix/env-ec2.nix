{ config, lib, pkgs, ... }:
with lib;
let
  user-data-script = ''
    curl="${pkgs.curl}/bin/curl -s --retry 3 --retry-delay 0 --fail"

    echo "running user-data script"
    $curl http://169.254.169.254/1.0/user-data | env PATH=$PATH:${pkgs.nettools}/bin ${pkgs.bash}/bin/bash
  '';
in
{
  imports = [
    <nixpkgs/nixos/modules/virtualisation/amazon-config.nix>
  ];

  config = {
    deployment.targetEnv = "ec2";
    ec2.metadata = true;

    system.activationScripts.ec2-apply-user-data = user-data-script;

    systemd.services.ec2-apply-user-data =
      { description = "EC2: apply user-data from RunInstances";

        wantedBy = [ "multi-user.target" ];
        before = [ "sshd.service" ];
        after = [ "fetch-ec2-data.service" ];

        script = user-data-script;

        serviceConfig.Type = "oneshot";
        serviceConfig.RemainAfterExit = true;
      };
  };
}
