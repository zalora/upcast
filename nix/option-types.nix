{ lib, ... }: with lib;
if (lib._upcast-option-types or false) != false then lib._upcast-option-types else rec {
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

  # ctor-set is a set from ctor names to the contained type. For example,
  # to represent data Foo = Foo Int | Bar String, you might have
  # types.adt { foo = types.int; bar = types.str; } as the type and
  # { foo = 2; } or { bar = "x"; } as the value.
  sum = ctor-set: mkOptionType {
    name = "an adt with these constructors: ${
      concatStringsSep " " (attrNames ctor-set)
    }";
    check = x: let
      names = attrNames x;

      ctor = head names;
    in isAttrs x && builtins.length names == 1 && ctor-set ? ${ctor} &&
      (ctor-set.${ctor}.check or (x: true)) x.${ctor};

    # Could theoretically merge matching ctors, but who cares
    merge = mergeOneOption;
  };
}
