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
    modules.desktop.mako.enable = true;
    security.polkit.enable = true;

    environment = {
      sessionVariables = {
        # required to fix issue where mouse is invisible
        WLR_NO_HARDWARE_CURSORS = "1";
        NIXOS_OZONE_WL = "1";

        # to make firefox look right
        GDK_SCALE = "3";
        GDK_DPI_SCALE = "1.5";
      };

      systemPackages = with pkgs; [
        awww
        wl-clipboard-rs
      ];
    };
  };
}
