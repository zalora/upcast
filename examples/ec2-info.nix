rec {
  args = {
    accessKeyId = "default";
    region = "eu-central-1";
    zone = "eu-central-1b";
    securityGroups = [ "sg-bbbbbbbb" ];
    subnet = "subnet-ffffffff";
    keyPair = "test-keypair";
    instanceProfileARN = "arn:aws:iam::555555555555:instance-profile/default";
  };

  # aws --region eu-central-1 ec2 describe-images --owners self | jq -r '.Images | map({key: .Name, value: .ImageId}) | from_entries'
  amis = {
    eu-central-1 = rec { 
      # should be any available NixOS ami for the chosen region
      default = "ami-12cff90f";
    };
  };
}
