{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.environment.niri;
in {
  imports = [
    ../hyprland/swww.nix
    ../../wayland
  ];

  options.modules.environment.niri = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    modules = {
      desktop = {
        wayland.enable = true;
        environment = {
          type = "wayland";
          hyprland.swww.enable = true;
        };
      };
    };

    programs.niri = {
      enable = true;
    };
  };
}
