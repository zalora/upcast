{ lib
, networks ? { whitelist = [ "8.8.8.8/32" "8.8.4.4/32" ]; }
}:
with lib;
[
  { protocol = "icmp";              sourceIp = "0.0.0.0/0"; }
  { fromPort = 22;  toPort = 22;    sourceIp = "0.0.0.0/0"; }
  { fromPort = 443; toPort = 443;   sourceIp = "0.0.0.0/0"; }
] ++ map (sourceIp: { protocol = "tcp"; fromPort = 0; toPort = 65535; inherit sourceIp; }) networks.whitelist
