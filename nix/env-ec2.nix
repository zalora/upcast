{ config, lib, pkgs, ... }:
with lib;
let
  base64 = "${pkgs.coreutils}/bin/base64";
  jq = "/usr/bin/env LD_LIBRARY_PATH=${pkgs.jq}/lib ${pkgs.jq}/bin/jq";
  curl = "${pkgs.curl}/bin/curl -s --retry 3 --retry-delay 0 --fail";
  wget = "${pkgs.wget}/bin/wget -q --retry-connrefused -O -";
  awk = "${pkgs.gawk}/bin/awk";
  openssl = "${pkgs.openssl}/bin/openssl";
  hostname = "${pkgs.nettools}/bin/hostname";
  ip = "${pkgs.iproute}/sbin/ip";
  bash = "${pkgs.bash}/bin/bash";

  hostname-script = ''
    set -- $(${curl} http://169.254.169.254/latest/user-data | ${jq} -r .hostname)
    echo setting hostname from EC2 metadata: $1
    ${hostname} $1
  '';

  register-hostname = {
    zoneId, zone, iamCredentialName,
    useLocalHostname, prefix ? if useLocalHostname then "local" else "public"
  }: pkgs.writeScript "ec2-register-hostname" ''
    ${ip} route delete blackhole 169.254.169.254 2>/dev/null || true

    date=$(${curl} -I https://route53.amazonaws.com/date | ${awk} '/^Date: / {sub("Date: ", "", $0); sub("\\r", "", $0); print $0}')

    set -- $(${wget} http://169.254.169.254/latest/meta-data/iam/security-credentials/${iamCredentialName} \
              | ${jq} -r '.SecretAccessKey, .AccessKeyId, .Token')

    signature=$(echo -n $date | ${openssl} dgst -binary -sha1 -hmac $1 | ${base64})
    auth_header="X-Amzn-Authorization: AWS3-HTTPS AWSAccessKeyId=$2,Algorithm=HmacSHA1,Signature=$signature"
    hostname=$(${hostname}).${zone}
    local_hostname=$(${wget} http://169.254.169.254/latest/meta-data/${prefix}-hostname)

    ${curl} -d @/dev/stdin \
          -H "Content-Type: text/xml" \
          -H "x-amz-date: $date" \
          -H "$auth_header" \
          -H "x-amz-security-token: $3" \
          -X POST https://route53.amazonaws.com/2013-04-01/hostedzone/${zoneId}/rrset <<__EOF
    <?xml version="1.0" encoding="UTF-8"?>
    <ChangeResourceRecordSetsRequest xmlns="https://route53.amazonaws.com/doc/2013-04-01/">
    <ChangeBatch>
       <Changes>
          <Change>
             <Action>UPSERT</Action>
             <ResourceRecordSet>
                <Name>$hostname</Name>
                <Type>CNAME</Type>
                <TTL>30</TTL>
                <ResourceRecords>
                   <ResourceRecord><Value>$local_hostname</Value></ResourceRecord>
                </ResourceRecords>
             </ResourceRecordSet>
          </Change>
       </Changes>
    </ChangeBatch>
    </ChangeResourceRecordSetsRequest>
    __EOF

    echo
    exit 0
  '';

  cfg = config.ec2;

  run-register-hostnames = concatStringsSep "\n" (map (script: ''
        ${bash} ${script}
        '')
      (mapAttrsToList (zone: args: register-hostname {
                       inherit zone;
                       inherit (args) zoneId iamCredentialName useLocalHostname;
                       }) cfg.route53RegisterHostname));

in
{
  imports = [
    <nixpkgs/nixos/modules/virtualisation/amazon-config.nix>
  ];

  options = {
    ec2.route53RegisterHostname = mkOption {
      type = types.attrsOf (types.submodule ({ lib, name, ... }: with lib; {
        options = {
          zoneId = mkOption { type = types.string; example = "ZOZONEZONEZONE"; };
          iamCredentialName = mkOption { type = types.string; example = "doge-iam-dns-profile"; };
          useLocalHostname = mkOption { type = types.bool; default = true; }; 
        };
      }));
      default = {};
    };
  };

  config = {
    deployment.targetEnv = "ec2";
    ec2.metadata = true;
    networking.firewall.enable = false;
    services.nixosManual.enable = false;

    system.activationScripts.ec2-apply-hostname = hostname-script + run-register-hostnames;

    systemd.services.ec2-apply-hostname = {
      description = "EC2: apply dynamic hostname";

      wantedBy = [ "multi-user.target" "sshd.service" ];
      before = [ "sshd.service" ];
      after = [ "fetch-ec2-data.service" ];

      script = hostname-script + run-register-hostnames;

      serviceConfig.Type = "oneshot";
      serviceConfig.RemainAfterExit = true;
    };
  };
}
