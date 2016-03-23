let
  lib = import <upcast/lib>;
  inherit (lib)
    concatStringsSep attrNames mapAttrs listToAttrs
    head tail length
    mapAttrsToList mapAttrsRecursive mapAttrsRecursiveCond
    filter last hasPrefix
    stringToCharacters optionalString toUpper concatStrings replaceChars stringLength
    splitString
    catAttrs concatMap attrValues isAttrs optionalAttrs foldAttrs filterAttrs;

  inherit (import <upcast/extralib.nix>) collect;

  fake-type = _type: { inherit _type; };
  fake-type-of = _type: _arg: {
    inherit _type _arg;
  };

  comma-prefix = "\n     , ";

  replace = replaceChars ["-"] [""];

  to-identifier = s:
    let
      chars = stringToCharacters s;
      hd = toUpper (head chars);
      tl = tail chars;
    in replace (hd + concatStrings tl);

  to-prefix = s: replace s + "_";
  to-suffix = replace;

  enclose = term:
    if (length (splitString " " term)) > 1
    then "(${toString term})" else "${toString term}";

  type-repr = v:
    if v ? _repr
    then (v._repr)
    else (v._type or "()") + optionalString ((v._arg or false) != false) " ${type-repr v._arg}";

  type-tag = v: if v ? _tag then v._tag else type-repr v;

  augment = key: v:
    if (v._type == "Record") then v // rec {
      options = mapAttrs augment v.options;

      _origname = v._name or key;
      _name = to-identifier _origname;
      _prefix = to-prefix _origname;
      _repr =
         concatStringsSep comma-prefix
            (mapAttrsToList (k: t: "${_prefix}${k} :: ${type-tag t}") options);

      _tag = to-identifier _name;
      _decl = ''
        data ${_name} = ${_name}
             { ${_repr}
             } deriving (Show, Generic)

        instance FromJSON ${_name} where
          parseJSON = genericParseJSON defaultOptions
                      { fieldLabelModifier = drop ${toString (stringLength _prefix)} }

        instance Hashable ${_name}
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
    } else if v._type == "InfraRef" then v // rec {
       _param = to-identifier v._name;
       _tag = "InfraRef ${_param}";
    } else if v._type == "Sum" then v // rec {
      _repr = concatStringsSep " | "
                (mapAttrsToList (ctor: arg:
                   "${to-identifier ctor}${optionalString (arg != null) " ${enclose (type-tag arg)}"}")
                   ctor-set);
      ctor-set = mapAttrs (k: v: if v != null then (augment k v) else v) v.ctor-set;
      _tag = to-identifier key;
      _decl = ''
        data ${_tag} = ${_repr} deriving (Show, Generic)

        instance FromJSON ${_tag} where
          parseJSON = genericParseJSON defaultOptions
                      { sumEncoding = ObjectWithSingleField
                      , constructorTagModifier = map toLower }

        instance Hashable ${_tag}
      '';
    } else v;


  inspectlib = {
    _upcast-option-types = rec {
      infra = type: rec {
        # specialized union
        _type = "InfraRef";
        _name = type;
      };
      sum = ctor-set: {
        _type = "Sum";
        inherit ctor-set;
      };
      inherit (inspectlib.types) submodule;
    };

    mkOptionType = abort "not enough overrides";
    mkOption = as: if as ? internal && as.internal then { _type = "Internal"; } else as.type;
    mkForce = lib.id;

    types = {
      nullOr = fake-type-of "Maybe";
      str = fake-type "Text";
      string = fake-type "Text";
      path = fake-type "Text";
      int = fake-type "Integer";
      bool = fake-type "Bool";
      attrsOf = fake-type-of "Attrs";
      listOf = fake-type-of "List";
      uniq = x: x;
      unspecified = fake-type "Value";
      submodule = function:
        let
          exp = function { lib = inspectlib; name = ""; config = {}; infra = {}; };
          self = optionalAttrs (exp ? config && exp.config ? _type) {
            _name = exp.config._type;
          };
        in { _type = "Record"; options = filterAttrs record-hack exp.options; } // self;
    };
  };

  # XXX: The 'Internal' trick seems unavoidable, but we should rig a way
  # to drop '_name' and '_type'.
  record-hack = n: v:
    n != "_name" &&
    n != "_type" &&
    (v ? _type && v._type != "Internal");

  toplevel = map (x:
    ((if builtins.typeOf x == "path" then import x else x)
     { infra = {}; config = {}; lib = inspectlib; }
    ).options
  ) (import <upcast/infra-types.nix> {}).imports;
  out = mapAttrs augment (foldAttrs (x: y: x // y) {} toplevel);

  cat = concatStringsSep "\n";
  decls =
    let
      to-as = xs: listToAttrs (map (x: { name = x._decl; value = x; }) xs);
    in to-as (collect (as: as ? _decl) out);
  # may also want a real uniqueness check here
  uniq-decls = attrNames decls;

  template = ''
    {-# LANGUAGE DeriveGeneric #-}

    module Infracast.NixTypes where

    --
    -- This file is autogenerated. Do not edit. I'm sorry.
    --

    import GHC.Generics
    import Control.Applicative
    import Data.Char (toLower)
    import Data.Hashable (Hashable(..))
    import Data.Text (Text)
    import Data.Map.Strict (Map, foldlWithKey)
    import Data.Aeson
    import Data.Aeson.Types

    type Attrs = Map Text

    ${cat uniq-decls}

    instance (Hashable a, Hashable b) => Hashable (Map a b) where
      hashWithSalt = foldlWithKey (\b k v -> hashWithSalt b (k,v))

    data InfraRef a = RefLocal Text | RefRemote Text deriving (Show, Generic)

    instance FromJSON (InfraRef a) where
      parseJSON (Object o) =
        (RefLocal <$> o .: "local") <|>
        (RefRemote <$> o .: "remote")
      parseJSON _ = empty

    instance Hashable (InfraRef a)'';
in template
