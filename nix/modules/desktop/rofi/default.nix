{ config, lib, pkgs, ... }:

# TODO make custom theme using city lights colors

with lib;
let
  cfg = config.modules.desktop.rofi;
  #inherit (config.lib.formats.rasi) mkLiteral;
in {
  options.modules.desktop.rofi = {
    enable = mkEnableOption false;
    theme = mkOption {
      description = "Rofi theme.";
      default = "android_notification";
      type = types.str;
    };
  };

  config = mkIf cfg.enable {
    home.programs.rofi = {
      enable = true;
      theme = cfg.theme;
    };
  };
}
