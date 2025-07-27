{
  config,
  lib,
  pkgs,
  ...
}:
# TODO make custom theme using city lights colors
with lib; let
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
      # theme = {
      #   "*" = {
      #     "selected-normal-foreground" =
      #       mkLiteral "rgba ( 255, 255, 255, 100 % )";
      #     "foreground" = mkLiteral "rgba ( 193, 193, 193, 100 % )";
      #     "normal-foreground" = mkLiteral "@foreground";
      #     "alternate-normal-background" =
      #       mkLiteral "rgba ( 39, 50, 56, 100 % )";
      #     "red" = mkLiteral "rgba ( 220, 50, 47, 100 % )";
      #     "selected-urgent-foreground" =
      #       mkLiteral "rgba ( 255, 24, 68, 100 % )";
      #     "blue" = mkLiteral "rgba ( 38, 139, 210, 100 % )";
      #     "urgent-foreground" = mkLiteral "rgba ( 255, 24, 68, 100 % )";
      #     "alternate-urgent-background" =
      #       mkLiteral "rgba ( 39, 50, 56, 100 % )";
      #     "active-foreground" = mkLiteral "rgba ( 128, 203, 196, 100 % )";
      #     "lightbg" = mkLiteral "rgba ( 238, 232, 213, 100 % )";
      #     "selected-active-foreground" =
      #       mkLiteral "rgba ( 128, 203, 196, 100 % )";
      #     "alternate-active-background" =
      #       mkLiteral "rgba ( 39, 50, 56, 100 % )";
      #     "background" = mkLiteral "rgba ( 39, 50, 56, 100 % )";
      #     "bordercolor" = mkLiteral "rgba ( 39, 50, 56, 100 % )";
      #     "alternate-normal-foreground" = mkLiteral "@foreground";
      #     "normal-background" = mkLiteral "rgba ( 39, 50, 56, 100 % )";
      #     "lightfg" = mkLiteral "rgba ( 88, 104, 117, 100 % )";
      #     "selected-normal-background" = mkLiteral "rgba ( 57, 66, 73, 100 % )";
      #     "border-color" = mkLiteral "@foreground";
      #     "spacing" = mkLiteral "2";
      #     "separatorcolor" = mkLiteral "rgba ( 30, 37, 41, 100 % )";
      #     "urgent-background" = mkLiteral "rgba ( 39, 50, 56, 100 % )";
      #     "selected-urgent-background" = mkLiteral "rgba ( 57, 66, 73, 100 % )";
      #     "alternate-urgent-foreground" = mkLiteral "@urgent-foreground";
      #     "background-color" = mkLiteral "rgba ( 0, 0, 0, 0 % )";
      #     "alternate-active-foreground" = mkLiteral "@active-foreground";
      #     "active-background" = mkLiteral "rgba ( 39, 50, 56, 100 % )";
      #     "selected-active-background" = mkLiteral "rgba ( 57, 66, 73, 100 % )";
      #   };
      # };
    };
  };
}
