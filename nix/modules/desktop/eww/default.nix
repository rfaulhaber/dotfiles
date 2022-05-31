{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.modules.desktop.eww;
  colors = config.modules.themes.colors;
  mapColorsToList = mapAttrsToList (name: value: "\$${name}: ${value};");
in {
  options.modules.desktop.eww = { enable = mkEnableOption false; };

  config = mkIf cfg.enable {
    home = {
      programs.eww = {
        enable = true;
        configDir = "${config.dotfiles.ewwDir}";
      };

      xdg.configFile."eww/theme".source = pkgs.writeTextFile {
        name = "theme.scss";
        text = concatStringsSep "\n" (mapColorsToList colors);
      };
    };
  };
}
