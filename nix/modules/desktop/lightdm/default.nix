{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.lightdm;
in {
  options.modules.desktop.lightdm = {
    enable = mkEnableOption false;
    background = mkOption {
      type = types.oneOf [types.str types.path];
      description = "Path for LightDM background.";
      default = pkgs.nixos-artwork.wallpapers.dracula.gnomeFilePath;
    };
    defaultSession = mkOption {
      type = types.str;
      description = "Default session passed to displayManager.";
    };
  };

  config = mkIf cfg.enable {
    services.xserver.displayManager = {
      lightdm = {
        enable = true;
        background = cfg.background;
      };

      defaultSession = cfg.defaultSession;
    };
  };
}
