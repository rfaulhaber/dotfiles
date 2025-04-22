{
  pkgs,
  inputs,
  ...
}: {
  imports =
    [
      "${toString pkgs.path}/nixos/modules/installer/sd-card/sd-image-aarch64-new-kernel-no-zfs-installer.nix"
      inputs.nixos-hardware.nixosModules.raspberry-pi-5
    ]
    ++ (with inputs.nixos-raspberrypi.nixosModules; [
      raspberry-pi-5.base
      raspberry-pi-5.display-vc4
    ]);

  formatAttr = "sdImage";
}
