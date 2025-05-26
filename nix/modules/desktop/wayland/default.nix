{
  inputs,
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
        GBM_BACKEND = "nvidia-drm";
        LIBVA_DRIVER_NAME = "nvidia"; # hardware acceleration
        __GLX_VENDOR_LIBRARY_NAME = "nvidia";
        NIXOS_OZONE_WL = "1";

        # to make firefox look right
        GDK_SCALE = "3";
        GDK_DPI_SCALE = "1.5";
      };

      systemPackages = with pkgs; [
        inputs.swww.packages.${pkgs.system}.swww
        wl-clipboard-rs
      ];
    };
  };
}
