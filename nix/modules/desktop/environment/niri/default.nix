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
        swww.enable = true;
        wayland.enable = true;
        environment = {
          type = "wayland";
        };
      };
    };

    programs.niri = {
      enable = true;
    };
  };
}
