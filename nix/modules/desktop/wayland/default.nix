{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.wayland;
in {
  options.modules.desktop.wayland = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      wl-clipboard
      mako
    ];
  };
}
