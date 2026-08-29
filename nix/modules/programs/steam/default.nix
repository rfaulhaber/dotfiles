{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.steam;
in {
  options.modules.programs.steam = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    programs.steam = {
      enable = true;
      # The desktop.wayland module exports GDK_SCALE=3 session-wide as a
      # firefox HiDPI workaround. Steam's UI is Chromium (steamwebhelper)
      # under XWayland, which also honors GDK_SCALE — tripling the client
      # while Wayland-native apps ignore it. Neutralize the GTK vars inside
      # the FHS env and size the client via Steam's own scaling knob.
      package = pkgs.steam.override {
        extraEnv = {
          GDK_SCALE = "1";
          GDK_DPI_SCALE = "1";
          STEAM_FORCE_DESKTOPUI_SCALING = "2";
        };
      };
    };
  };
}
