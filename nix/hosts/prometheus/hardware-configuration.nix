# Filesystems are declared in disko.nix and materialized by nixos-anywhere;
# kernel, firmware, bootloader, and initrd modules come from nixos-raspberrypi's
# raspberry-pi-5 module. This file exists to hold any host-specific hardware
# quirks that surface after installation (e.g. extra kernel modules for a hat).
{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.initrd.availableKernelModules = [];
  boot.initrd.kernelModules = [];
  boot.kernelModules = [];
  boot.extraModulePackages = [];

  swapDevices = [];

  nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";
}
