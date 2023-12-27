{
  pkgs,
  bootloader,
  ...
}: {
  imports = ["${toString pkgs.path}/nixos/modules/installer/sd-card/sd-image-aarch64-installer.nix"];

  formatAttr = "sdImage";

  sdImage = {
    compressImage = false;

    populateFirmwareCommands = "";

    postBuildCommands = ''
      echo "DEBUG: flashing bootloader"
      dd if=${bootloader}/bootloader.bin of=$img conv=fsync,notrunc bs=512 seek=64 status=progress
      echo "DEBUG: flashing bootloader finished"
    '';
  };
}
