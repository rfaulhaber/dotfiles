{
  pkgs,
  ...
}: {
  imports = ["${toString pkgs.path}/nixos/modules/installer/sd-card/sd-image-aarch64-installer.nix"];
  formatAttr = "sdImage";

  nixpkgs.config.allowUnfree = true; # needed for ubootRock64

  sdImage.compressImage = false;

  # at the time of writing the u-boot version from FireFly hasn't been successfully ported yet
  # so we use the one from Rock64
  sdImage.postBuildCommands = with pkgs; ''
    dd if=${ubootRock64}/idbloader.img of=$img conv=fsync,notrunc bs=512 seek=64
    dd if=${ubootRock64}/u-boot.itb of=$img conv=fsync,notrunc bs=512 seek=16384
  '';
}
