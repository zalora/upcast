let 

  trivial = import ./trivial.nix;
  lists = import ./lists.nix;
  strings = import ./strings.nix;
  attrsets = import ./attrsets.nix;
  modules = import ./modules.nix;
  options = import ./options.nix;
  types = import ./types.nix;
  meta = import ./meta.nix;
  debug = import ./debug.nix;
  misc = import ./misc.nix;

in
  { inherit trivial lists strings attrsets options
      modules types meta debug;
  }
  // trivial // lists // strings // attrsets
  // options // types // meta // debug // misc // modules
