{
  config,
  lib,
  pkgs,
  isLinux,
  ...
}:
with lib; let
  cfg = config.modules.programs.steam;
in {
  options.modules.programs.steam = {enable = mkEnableOption false;};

  # NixOS-only options. Defining them on darwin fails evaluation even under a
  # false mkIf, because the option itself is undeclared there.
  config = mkIf cfg.enable (optionalAttrs isLinux {
    programs.steam = {
      enable = true;

      # Steam discovers non-stock Proton only via STEAM_EXTRA_COMPAT_TOOLS_PATHS,
      # which these options weave into the package env — there is no $PATH
      # fallback, so a proton-ge-bin or protontricks installed anywhere else
      # would be invisible to it.
      extraCompatPackages = [pkgs.proton-ge-bin];
      protontricks.enable = true;

      # Steam's UI is Chromium (steamwebhelper) under XWayland, which honors
      # GTK scaling vars. Pin them to neutral so stray session scaling can
      # never leak in (it did once: notes/hidpi_scaling.org), and size the
      # client via Steam's own knob instead.
      package = pkgs.steam.override {
        extraEnv = {
          GDK_SCALE = "1";
          GDK_DPI_SCALE = "1";
          STEAM_FORCE_DESKTOPUI_SCALING = "2";
        };
      };
    };
  });
}
