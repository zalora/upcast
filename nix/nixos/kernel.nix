{ pkgs }:
rec {
  cleanLinux =
    kernel:
    let
      args.buildInputs = with pkgs; [findutils gawk bash];
      mod = "lib/modules/${kernel.version}";
    in
    pkgs.runCommand "upcast-cloud-linux-${kernel.version}" args ''
      mkdir -p $out

      cp -v ${kernel}/{bzImage,System.map} $out
      mkdir -p $out/${mod}
      cp -v ${kernel}/${mod}/modules.* $out/${mod}/

      bash ${./kernel/module-inclusion} {${kernel},$out}/${mod}/kernel ${./kernel/generic.inclusion-list}

      find $out/${mod} -type f | awk '
        FILENAME=="-" {
          split($0, fnp, /\//);
          key = fnp[length(fnp)];
          if (rm[key]) print;
        }
        FILENAME!="-" {
          rm[$1] = $1;
        }
        ' ${./kernel/exclude.amd64-virtual} - | xargs -t rm
  '';
}
