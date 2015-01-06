{ pkgs }:
rec {
  cleanLinux =
    kernel: exclude:
    let
      args = {
        buildInputs = with pkgs; [findutils gawk bash];
        features = "";
      };
      mod = "lib/modules/${kernel.modDirVersion}";
    in
    pkgs.runCommand "upcast-cloud-linux-${kernel.version}" args (''
      mkdir -p $out

      cp -v ${kernel}/{bzImage,System.map} $out
      mkdir -p $out/${mod}
      cp -v ${kernel}/${mod}/modules.* $out/${mod}/

      # include modules selected by ubuntu guys
      bash ${./kernel/module-inclusion} {${kernel},$out}/${mod}/kernel ${./kernel/generic.inclusion-list}
    '' + pkgs.lib.optionalString exclude ''
      # exclude virtual-flavor modules selected by ubuntu guys
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
  '');
}
