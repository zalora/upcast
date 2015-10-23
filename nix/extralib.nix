let
  lib = import <upcast/lib>;
  inherit (lib) isAttrs concatMap attrValues filterAttrs mapAttrs;
in
rec {
  # like lib.collect but doesn't stop recursing when predicate matches
  collect = pred: attrs:
    if pred attrs && isAttrs attrs then
      [ attrs ] ++ concatMap (collect pred) (attrValues attrs)
    else if isAttrs attrs then
      concatMap (collect pred) (attrValues attrs)
    else [];

  filterAttrsRecursive = pred: value:
    if builtins.isAttrs value then
      mapAttrs (_: filterAttrsRecursive pred) (filterAttrs (_: pred) value)
    else value;
}
