{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.fuzzel;
  colors = config.modules.themes.colors;
  font = "Hack Nerd Font Mono";
in {
  options.modules.desktop.fuzzel = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      fuzzel
    ];

    home.configFile."fuzzel/fuzzel.ini".text = ''
      terminal = ghostty -e
      font = ${font}

      [colors]
      background=${colors.base00}EE
      text=${colors.fg}FF
      match=${colors.red}FF
      selection=${colors.fg-alt}FF
      selection-text=${colors.cyan}FF
      selection-match=${colors.red}FF
      border=${colors.teal}FF
    '';
  };
}
