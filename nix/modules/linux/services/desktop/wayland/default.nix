{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.wayland;
in {
  options.modules.desktop.wayland = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    security.polkit.enable = true;

    environment = {
      sessionVariables = {
        # required to fix issue where mouse is invisible
        WLR_NO_HARDWARE_CURSORS = "1";
        NIXOS_OZONE_WL = "1";
      };

      systemPackages = with pkgs; [
        awww
        wl-clipboard-rs
      ];
    };
  };
}
