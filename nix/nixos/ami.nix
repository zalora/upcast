{ pkgs ? import <nixpkgs-tools> { system = "x86_64-linux"; config.allowUnfree = true; }
, lib ? pkgs.lib

, aws-env ? {}

, context ? rec {
    region = "eu-west-1";
    bucket = "upcast-${region}";
  }
, ... }:
with lib;

let
  env =
    let
      getEnvs = xs: listToAttrs (map (x: nameValuePair x (builtins.getEnv x)) xs);

      base1 = getEnvs [ "AWS_ACCOUNT_ID" "AWS_X509_CERT" "AWS_X509_KEY" ];
      base2 = getEnvs [ "AWS_ACCESS_KEY" "AWS_SECRET_KEY" ];
      more = {
        AWS_ACCESS_KEY_ID = env.AWS_ACCESS_KEY;
        AWS_SECRET_ACCESS_KEY = env.AWS_SECRET_KEY;
      };
    in base1 // base2 // more // aws-env // { __noChroot = true; };

  ec2-bundle-image = "${pkgs.ec2_ami_tools}/bin/ec2-bundle-image";
  ec2-upload-bundle = "${pkgs.ec2_ami_tools}/bin/ec2-upload-bundle";
  awscli = "${pkgs.awscli}/bin/aws";

  nixos-hvm = let
      hvm-config = { config, ... }: {
          imports = [ ./env-ec2.nix ];
          ec2.hvm = true; # pv is almost past
      };
    in (import ./. { configuration = hvm-config; });

  vbox = let
      vbox-config = { config, ... }: {
          imports = [ ./env-virtualbox.nix ];
      };
    in (import ./. { configuration = vbox-config; }).config.system.build.virtualBoxImage;

  image = nixos-hvm.config.system.build.amazonImage;
  image-name = "$(basename ${image})-nixos-upcast";
in
rec {
  inherit (nixos-hvm) vm vmWithBootLoader;
  inherit image vbox;

  bundle = pkgs.runCommand "ami-ec2-bundle-image" env ''
    mkdir -p $out

    ${ec2-bundle-image} \
      -c "$AWS_X509_CERT" -k "$AWS_X509_KEY" -u "$AWS_ACCOUNT_ID" \
      -i "${image}/nixos.img" --arch x86_64 -d $out
  '';

  upload = pkgs.runCommand "ami-ec2-upload-image" env ''
    export PATH=${pkgs.curl}/bin:$PATH
    export CURL_CA_BUNDLE=${pkgs.cacert}/etc/ca-bundle.crt

    ${ec2-upload-bundle} \
      -b "${context.bucket}/${image-name}" \
      -d ${bundle} -m ${bundle}/nixos.img.manifest.xml \
      -a "$AWS_ACCESS_KEY" -s "$AWS_SECRET_KEY" --region ${context.region}

    echo "${context.bucket}/${image-name}/nixos.img.manifest.xml" > $out
  '';

  register = pkgs.runCommand "ami-ec2-register-image" env ''
    set -o pipefail

    ${awscli} ec2 register-image \
      --region "${context.region}" \
      --name "${image-name}" \
      --description "${image-name}" \
      --image-location "$(cat ${upload})" \
      --virtualization-type "hvm" | ${pkgs.jq}/bin/jq -r .ImageId > $out || rm -f $out
    cat $out
  '';
}
