# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ehci_pci"
    "ahci"
    "mpt3sas"
    "nvme"
    "usb_storage"
    "usbhid"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/61189968-f3ef-424e-95a4-45e47a6564d0";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/113F-0AA3";
    fsType = "vfat";
  };

  fileSystems."/diskp" = {
    device = "/dev/disk/by-uuid/cd63adbc-2a15-4982-bf4d-73e66ede8e91";
    fsType = "ext4";
  };

  fileSystems."/disk1" = {
    device = "/dev/disk/by-uuid/b604ac55-acca-430e-a0b3-2dabe90b0e43";
    fsType = "ext4";
  };

  fileSystems."/disk2" = {
    device = "/dev/disk/by-uuid/9a3d8bb7-5cf9-4477-ae58-d1266b8f9b25";
    fsType = "ext4";
  };

  fileSystems."/disk3" = {
    device = "/dev/disk/by-uuid/3e5d4b1c-f88a-4632-b3fb-973346387b22";
    fsType = "ext4";
  };

  fileSystems."/disk4" = {
    device = "/dev/disk/by-uuid/90f2f6e1-f49d-4acf-bae8-394e1356959a";
    fsType = "ext4";
  };

  fileSystems."/disk5" = {
    device = "/dev/disk/by-uuid/a9df50f5-faa4-4093-8e80-24872f6463ce";
    fsType = "ext4";
  };

  fileSystems."/disk6" = {
    device = "/dev/disk/by-uuid/e325d871-5d0c-4291-8a62-a7f2e4d0ab6f";
    fsType = "ext4";
  };

  fileSystems."/disk7" = {
    device = "/dev/disk/by-uuid/2d010b24-0238-4740-a942-703a3c434461";
    fsType = "ext4";
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/1d06ce0b-9c0c-4813-8272-f613efeb10b8";
    fsType = "ext4";
  };

  swapDevices = [ ];

  hardware.cpu.intel.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;
  # high-resolution display
  hardware.video.hidpi.enable = lib.mkDefault true;
}
