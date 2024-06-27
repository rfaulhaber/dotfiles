{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.hardware.zsa;
in {
  options.modules.hardware.zsa = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    # I only own the Moonlander and Planck-EZ keyboards, which use the same config
    services.udev.extraRules = ''
      SUBSYSTEM=="usb", ATTR{idVendor}=="3297", ATTR{idProduct}=="1969", GROUP="plugdev"
    '';

    environment.systemPackages = with pkgs; [keymapp];
  };
}
