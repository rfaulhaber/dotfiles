# TODO make this module more modular. create pre-defined configs and allow them to be flipped on and off
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.waybar;
  themeCfg = config.modules.themes;
  c = themeCfg.colors.withHashtag;

  waybarScssSource = ./style.scss;

  # Compile style.scss with dart-sass, injecting the centralized theme as theme.scss
  waybarStyle =
    pkgs.runCommand "waybar-style-css" {
      nativeBuildInputs = [pkgs.dart-sass];
    } ''
      mkdir -p $out theme
      echo ${escapeShellArg themeCfg.scss} > theme/theme.scss
      sass --no-source-map \
        --load-path=theme \
        ${waybarScssSource} \
        $out/style.css
    '';

  # Waybar JSON config as a Nix attrset
  waybarConfig = import ./config.nix {
    colors = c;
    homePath = config.user.home;
  };
in {
  options.modules.desktop.waybar = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    user.packages = [
      inputs.waybar.packages.${pkgs.stdenv.hostPlatform.system}.default
    ];

    home.configFile = {
      "waybar/config".text = builtins.toJSON waybarConfig;
      "waybar/style.css".source = "${waybarStyle}/style.css";
    };
  };
}
