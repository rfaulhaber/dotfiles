# TODO refactor
{
  pkgs,
  bootloader,
  ...
}: {
  # this configuration is an extension of the generic sd-image provided by nix,
  # since the default aarch64 installer is tailored for raspberry pi only
  imports = ["${toString pkgs.path}/nixos/modules/installer/sd-card/sd-image-aarch64-new-kernel-no-zfs-installer.nix"];

  formatAttr = "sdImage";

  # NOTE: it is requried to modify the kernel params after the disk has been mounted to just this
  # for some reason I can't override the default ones
  boot.kernelParams = ["console=tty1" "console=ttyS2,1500000n8"];

  sdImage = {
    compressImage = false;

    # not sure if this is necessary
    populateFirmwareCommands = ''
      cp ${bootloader}/bootloader.bin firmware/roc-rk3328-cc.bin
    '';

    # recreates libre computer flash tool
    postBuildCommands = ''
      dd if=${bootloader}/bootloader.bin of=$img conv=fsync,notrunc bs=512 seek=64 status=progress
    '';
  };
}
