{ config, pkgs, lib, ... }: with lib;
let
  cloudDefault = mkOverride 999;
in
{
    # usually covered by things like security groups
    networking.firewall.enable = cloudDefault false;

    # likely not needed on a cloud box
    services.nixosManual.enable = cloudDefault false;
    services.openssh.enable = cloudDefault true;
    services.openssh.passwordAuthentication = cloudDefault false;
    services.openssh.challengeResponseAuthentication = cloudDefault false;
    services.udisks2.enable = cloudDefault false;
    security.polkit.enable = cloudDefault false;
    environment.noXlibs = cloudDefault true;
    time.timeZone = cloudDefault "UTC";
    i18n.supportedLocales = cloudDefault ["en_US.UTF-8/UTF-8"];
}
