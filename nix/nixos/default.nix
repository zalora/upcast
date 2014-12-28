{ configuration
, system ? "x86_64-linux"
, modfilter ? (x: x != <nixpkgs/nixos/modules/programs/info.nix>)
}:

let
  eval-config = import <nixpkgs/nixos/lib/eval-config.nix>;
  baseModules = builtins.filter modfilter (import <nixpkgs/nixos/modules/module-list.nix>);

  eval = eval-config {
    inherit system baseModules;
    modules = [ configuration ];
  };

  inherit (eval) pkgs;

  vm-modules = [ configuration <nixpkgs/nixos/modules/virtualisation/qemu-vm.nix> ];

  vm = (eval-config {
    inherit system baseModules;
    modules = vm-modules;
  }).config;

  vm-bootloader = (eval-config {
    inherit system baseModules;
    modules = vm-modules ++ [ { virtualisation.useBootLoader = true; } ];
  }).config;

in
{
  inherit (eval) config options;

  system = eval.config.system.build.toplevel;

  vm = vm.config.system.build.vm;

  vmWithBootLoader = vm-bootloader.system.build.vm;
}
