{
  modulesPath,
  inputs,
  pkgs,
  ...
}: {
  imports = [
    "${toString pkgs.path}/nixos/modules/installer/sd-card/sd-image-aarch64-new-kernel-no-zfs-installer.nix"
    inputs.nixos-hardware.nixosModules.raspberry-pi-3
  ];

  formatAttr = "sdImage";
}
