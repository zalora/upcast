let
  lib = import <nixpkgs/lib>;
  inherit (lib)
    concatStringsSep attrNames mapAttrs
    head tail length
    mapAttrsToList mapAttrsRecursive mapAttrsRecursiveCond
    filter last hasPrefix
    stringToCharacters optionalString toUpper concatStrings replaceChars stringLength
    splitString
    catAttrs concatMap attrValues isAttrs;

  collect = pred: attrs:
    if pred attrs && isAttrs attrs then
      [ attrs ] ++ concatMap (collect pred) (attrValues attrs)
    else if isAttrs attrs then
      concatMap (collect pred) (attrValues attrs)
    else [];

  fake-type = _type: { inherit _type; };
  fake-type-of = _type: _arg: {
    inherit _type _arg;
  };

  replace = replaceChars ["-"] [""];

  to-identifier = s:
    let
      chars = stringToCharacters s;
      hd = toUpper (head chars);
      tl = tail chars;
    in replace (hd + concatStrings tl);

  to-prefix = s: replace s + "_";

  enclose = term:
    if (length (splitString " " term)) > 1
    then "(${toString term})" else "${toString term}";

  type-repr = v:
    if ((v._repr or false) != false)
    then (v._repr)
    else (v._type or "()") + optionalString ((v._arg or false) != false) " ${type-repr v._arg}";

  type-tag = v:
    if ((v._tag or false) != false)
    then (v._tag)
    else type-repr v;

  augment = key: v:
    if (v._type == "Record") then v // rec {
      options = mapAttrs augment v.options;

      _origname = if v._name == "AnonModule" then key else v._name;
      _name = to-identifier _origname;
      _prefix = to-prefix _origname;
      _repr =
         concatStringsSep "\n      , "
            (mapAttrsToList (k: t: "${_prefix}${k} :: ${type-tag t}") options);

      _tag = (to-identifier _name);
      _decl = ''
        data ${_name} = ${_name}
              { ${_repr}
              } deriving (Show, Generic)

        instance FromJSON ${_name} where
          parseJSON = genericParseJSON defaultOptions
                      { fieldLabelModifier = drop ${toString (stringLength _prefix)} }

        instance ToJSON ${_name} where
          toJSON = genericToJSON defaultOptions
                   { fieldLabelModifier = drop ${toString (stringLength _prefix)} }

      '';
    } else if (v._type == "List") then v // rec {
      _repr = "[${type-tag _arg}]";
      _arg = augment key v._arg;
    } else if (v._type == "Attrs" || v._type == "Maybe") then v // rec {
      _arg = augment key v._arg;
      _repr = "${v._type} ${enclose (type-tag _arg)}";
      #_tag = to-identifier key;
      #_decl = "type ${_tag} = ${_repr}";
    } else if v._type == "Either" then v // rec {
       _repr = "Either ${type-tag left} ${type-tag right}";
       left = augment key v.left;
       right = augment key v.right;
       #_tag = to-identifier key;
       #_decl = "type ${_tag} = ${_repr}";
    } else if v._type == "RecordRef" then v // {
       _tag = to-identifier v._name;
       _name = to-identifier v._name;
    } else if v._type == "Sum" then v // rec {
      _repr = concatStringsSep " | "
                (mapAttrsToList (ctor: arg:
                   "${to-identifier ctor}${optionalString (arg != null) " ${enclose (type-tag arg)}"}")
                   ctor-set);
      ctor-set = mapAttrs (k: v: if v != null then (augment k v) else v) v.ctor-set;
      _tag = to-identifier key;
      _decl = ''
        data ${_tag} = ${_repr} deriving (Show, Generic)
        instance FromJSON ${_tag}
        instance ToJSON ${_tag}
      '';
    } else v;


  inspectlib = {
    _upcast-option-types = rec {
      union = t1: t2: rec {
        _type = "Either";
        left = t1;
        right = t2;
      };
      infra = type: union inspectlib.types.str rec {
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
          self = {
            _name = exp.config._type or "AnonModule";
          };
        in { _type = "Record"; options = exp.options; } // self;
    };
  };

  toplevel-map = _: { baseModules, ... }: inspectlib.types.submodule (import (head baseModules));
  toplevel = mapAttrs toplevel-map (import ./infra-types.nix);
  out = mapAttrs augment toplevel;

  strip-term = term:
    mapAttrsRecursive (keypath: t1: if builtins.isFunction t1 then (t1 keypath) else t1) term;

  cat = concatStringsSep "\n";
  decls = catAttrs "_decl" (collect (as: as ? _decl) out);

in ''
{-# LANGUAGE DeriveGeneric #-}

module Upcast.Infra.Nix where

--
-- This file is autogenerated. Do not edit.
--

import GHC.Generics
import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.Aeson
import Data.Aeson.Types

type Attrs = Map Text


${cat decls}
''
