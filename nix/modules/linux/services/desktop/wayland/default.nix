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

        # to make firefox look right — but session-wide, so it leaks into any
        # XWayland app whose toolkit reads it (Chromium/CEF: see the Steam
        # module). Scope corrections per-app; recipe in notes/hidpi_scaling.org
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
