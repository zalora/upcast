{ lib, ... }: with lib;
let
   mergeOneTransform = xform-f: loc: defs:
    if defs == [] then abort "This case should never happen."
    else if length defs != 1 then
      throw "The unique option `${showOption loc}' is defined multiple times, in ${showFiles (getFiles defs)}."
    else xform-f (head defs).value;
in
if (lib._upcast-option-types or false) != false then lib._upcast-option-types else rec {
  infra = type: mkOptionType {
    name = "infra of type ‘${type}’ or remote resource id";
    check = x: x._type or "" == type || isString x;
    merge = mergeOneTransform (x: if x ? _type then { local = x._name; } else { remote = x; });
  };

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

    # matching aeson's ObjectWithSingleField
    merge = mergeOneTransform (mapAttrs (_: v: if v == null then [] else v));
  };
}
