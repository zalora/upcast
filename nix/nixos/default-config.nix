{ config, pkgs, lib, ... }: with lib;
let
  cloudDefault = mkOverride 900;
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

    environment.noXlibs = cloudDefault true;
    time.timeZone = cloudDefault "UTC";
    i18n.supportedLocales = cloudDefault ["en_US.UTF-8/UTF-8"];

    security.polkit.enable = cloudDefault false;
    security.pam.loginLimits = [ # login sessions only, not systemd
      { domain = "*"; type = "hard"; item = "core"; value = "1048576"; }
      { domain = "*"; type = "soft"; item = "core"; value = "1048576"; } # one gigabyte

      { domain = "*"; type = "soft"; item = "nofile"; value = "262140"; }
      { domain = "*"; type = "hard"; item = "nofile"; value = "524280"; }
    ];

    systemd.extraConfig = ''
      DefaultLimitCORE=1048576
    '';

    boot.tmpOnTmpfs = cloudDefault true;
    boot.cleanTmpDir = cloudDefault true;

    boot.kernel.sysctl = {
      # allows control of core dumps with systemd-coredumpctl
      "kernel.core_pattern" = cloudDefault "|${pkgs.systemd}/lib/systemd/systemd-coredump %p %u %g %s %t %e";

      "fs.file-max" = cloudDefault "524280";

      # moar ports
      "net.ipv4.ip_local_port_range" = cloudDefault "10000 65000";

      # should be the default, really
      "net.ipv4.tcp_slow_start_after_idle" = cloudDefault "0";
      "net.ipv4.tcp_early_retrans" = cloudDefault "1"; # 3.5+

      # backlogs
      "net.core.netdev_max_backlog" = cloudDefault "4096";
      "net.core.somaxconn" = cloudDefault "4096";

      # tcp receive flow steering (newer kernels)
      "net.core.rps_sock_flow_entries" = cloudDefault "32768";

      # max bounds for buffer autoscaling (16 megs for 10 gbe)
      #"net.core.rmem_max" = cloudDefault "16777216";
      #"net.core.wmem_max" = cloudDefault "16777216";
      #"net.core.optmem_max" = cloudDefault "40960";
      #"net.ipv4.tcp_rmem" = cloudDefault "4096 87380 16777216";
      #"net.ipv4.tcp_wmem" = cloudDefault "4096 65536 16777216";

      # vm
      #"vm.overcommit_memory" = lib.mkDefault "2"; # no overcommit
      #"vm.overcommit_ratio" = "100";
      "vm.swappiness" = cloudDefault "10"; # discourage swap

      # just in case for postgres and friends
      "kernel.msgmnb" = cloudDefault "65536";
      "kernel.msgmax" = cloudDefault "65536";
      "kernel.shmmax" = cloudDefault "68719476736";
    };
}
