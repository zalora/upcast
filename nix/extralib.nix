let
  lib = import <upcast/lib>;
  inherit (lib) isAttrs concatMap attrValues;
in
rec {
  # like lib.collect but doesn't stop recursing when predicate matches
  collect = pred: attrs:
    if pred attrs && isAttrs attrs then
      [ attrs ] ++ concatMap (collect pred) (attrValues attrs)
    else if isAttrs attrs then
      concatMap (collect pred) (attrValues attrs)
    else [];
}
