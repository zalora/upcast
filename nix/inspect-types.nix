let
  lib = import <nixpkgs/lib>;
  inherit (lib) concatStringsSep attrNames mapAttrs head;

  fake-type = _type: { inherit _type; };
  fake-type-of = _type: _arg: { inherit _type _arg; };

  inspectlib = {
    _upcast-option-types = rec {
      union = t1: t2: {
        _type = "Either";
        left = t1;
        right = t2;
      };
      infra = type: union inspectlib.types.str {
        _type = "RecordRef";
        _name = type;
      };
      sum = ctor-set: {
        _type = "Sum";
        inherit ctor-set;
      };
    };

    mkOptionType = abort "not enough overrides";
    mkOption = as: as.type;

    types = {
      nullOr = fake-type-of "Maybe";
      str = fake-type "Text";
      string = fake-type "Text";
      path = fake-type "Text";
      int = fake-type "Int";
      bool = fake-type "Bool";
      attrsOf = fake-type-of "Attrs";
      listOf = fake-type-of "List";
      uniq = x: x;
      unspecified = fake-type "Value";
      submodule = function:
        let
          exp = function { lib = inspectlib; name = ""; config = {}; };
          _name = exp.config._type or "AnonModule";
        in { _type = "Record"; inherit _name; options = exp.options; };
    };
  };

  toplevel-map = _: { baseModules, ... }: inspectlib.types.submodule (import (head baseModules));
in mapAttrs toplevel-map (import ./infra-types.nix)
