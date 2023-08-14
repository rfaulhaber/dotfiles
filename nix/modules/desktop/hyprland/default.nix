{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.hyprland;
in {
  options.modules.desktop.hyprland = {
    enable = mkEnableOption false;
    enableNvidiaPatches = mkEnableOption false;
    extraStartupPrograms = mkOption {
      default = [];
      type = types.listOf types.str;
      description = "Extra programs to start upon launch.";
    };
  };

  config = mkIf cfg.enable {
    # not great
    services.xserver.displayManager.gdm = {
      enable = true;
      wayland = true;
    };

    programs.hyprland.enable = true;

    home.wayland.windowManager.hyprland = {
      enable = true;
      enableNvidiaPatches = cfg.enableNvidiaPatches;
      systemdIntegration = true;
      settings = let
        emacsclientExec = "${pkgs.emacs}/bin/emacsclient";
        firefoxExec = "${pkgs.firefox-devedition-bin}/bin/firefox-devedition";
      in {
        "$mod" = "SUPER";

        bind = [
          "$mod, enter, exec, kitty"
          "$mod, e, exec, ${emacsclientExec}"
          "$mod, b, exec, firefox"
          "$mod ALT, s, execr ${pkgs.gnome.gnome-screenshot}/bin/gnome-screenshot -i"
          "$mod ALT, e, execr ${emacsclientExec} -e '(emacs-everywhere)"
        ];
      };
    };
  };
}
