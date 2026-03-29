{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.mako;
  colors = config.modules.themes.colors.withHashtag;
  font = config.modules.themes.font;
in {
  options.modules.desktop.mako = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    user.packages = with pkgs; [mako];

    home.configFile."mako/config".text = ''
      font=${font} 16
      background-color=${colors.base07}FF
      text-color=${colors.base00}FF
    '';
  };
}
