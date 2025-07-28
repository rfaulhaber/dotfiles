{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.environment.gnome;
in {
  options.modules.desktop.environment.gnome = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    modules.desktop = {
      xserver.enable = true;
      environment.type = "x11";
    };

    services = {
      desktopManager.gnome.enable = true;
      displayManager.gdm.enable = true;
    };
  };
}
