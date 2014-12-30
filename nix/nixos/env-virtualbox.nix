# used to be nixops/nix/virtualbox-image-nixops.nix
{ config, pkgs, lib, ... }:

let
  clientKeyPath = "/root/.vbox-nixops-client-key";
  inherit (import ./kernel.nix { inherit pkgs; }) cleanLinux;
  cloudKernel = pkgs.linuxPackages_3_14 // { kernel = cleanLinux pkgs.linux_3_14 false; };
in
{
  require = [
    <nixpkgs/nixos/modules/virtualisation/virtualbox-image.nix>
    ./default-config.nix
  ];

  jobs."get-vbox-nixops-client-key" =
    { description = "Get NixOps SSH Key";
      wantedBy = [ "multi-user.target" ];
      before = [ "sshd.service" ];
      requires = [ "dev-vboxguest.device" ];
      after = [ "dev-vboxguest.device" ];
      path = [ config.boot.kernelPackages.virtualboxGuestAdditions ];
      preStart =
        ''
          set -o pipefail
          VBoxControl -nologo guestproperty get /VirtualBox/GuestInfo/Charon/ClientPublicKey | sed 's/Value: //' > ${clientKeyPath}.tmp
          mv ${clientKeyPath}.tmp ${clientKeyPath}

          if [[ ! -f /etc/ssh/ssh_host_ecdsa_key ]]; then
            VBoxControl -nologo guestproperty get /VirtualBox/GuestInfo/Charon/PrivateHostKey | sed 's/Value: //' > /etc/ssh/ssh_host_ecdsa_key.tmp
            mv /etc/ssh/ssh_host_ecdsa_key.tmp /etc/ssh/ssh_host_ecdsa_key
            chmod 0600 /etc/ssh/ssh_host_ecdsa_key
          fi
        '';
    };

  services.openssh.authorizedKeysFiles = [ ".vbox-nixops-client-key" ];

  boot.vesa = false;

  boot.loader.grub.timeout = 1;

  boot.kernelPackages = cloudKernel;

  # VirtualBox doesn't seem to lease IP addresses persistently, so we
  # may get a different IP address if dhcpcd is restarted.  So don't
  # restart dhcpcd.
  systemd.services.dhcpcd.restartIfChanged = false;
}
