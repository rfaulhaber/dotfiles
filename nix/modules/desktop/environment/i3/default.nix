{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.environment.i3;
in {
  options.modules.desktop.environment.i3 = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    # TODO fill me out!
    services.xserver.windowManager.i3 = {
      enable = true;
      configFile = "${config.dotfiles.configDir}/i3";
    };

    modules.desktop.lightdm = {
      enable = true;
      defaultSession = "none+i3";
    };
  };
}
