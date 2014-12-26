{ lib, ... }: with lib;
rec {
  union = t1: t2: mkOptionType {
    name = "${t1.name} or ${t2.name}";
    check = x: t1.check x || t2.check x;
    merge = mergeOneOption;
  };

  infra = type: union types.str (mkOptionType {
    name = "infra of type ‘${type}’";
    check = x: x._type or "" == type;
    merge = mergeOneOption;
  });
}
