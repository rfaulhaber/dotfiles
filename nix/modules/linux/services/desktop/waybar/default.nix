# TODO make this module more modular. create pre-defined configs and allow them to be flipped on and off
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  inherit (builtins) concatStringsSep map readFile;
  cfg = config.modules.desktop.waybar;
  themeCfg = config.modules.themes;
  c = themeCfg.colors.withHashtag;

  # Generate a :root block with CSS custom properties from the theme.
  # The static style.css references these as var(--background), var(--base07), etc.
  rootBlock = let
    vars =
      themeCfg.themeAttrs
      |> attrsToList
      |> (map ({
        name,
        value,
      }: "    --${name}: ${value};"))
      |> (concatStringsSep "\n");
  in ''
    :root {
    ${vars}
        --font-family: ${themeCfg.font};
    }
  '';

  waybarCss = rootBlock + readFile ./style.css;

  # Waybar JSON config as a Nix attrset
  waybarConfig = import ./config {
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
      "waybar/style.css".text = waybarCss;
    };
  };
}
