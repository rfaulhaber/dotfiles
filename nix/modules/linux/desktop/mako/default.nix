{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.mako;
in {
  options.modules.desktop.mako = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    user.packages = with pkgs; [mako];

    home.file.makoconf = {
      source = "${config.dotfiles.configDir}/mako/config";
      target = "${config.user.home}/.config/mako/config";
    };
  };
}
