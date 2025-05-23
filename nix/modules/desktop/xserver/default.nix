# this module is for common xserver settings for all xserver-based desktop environments
# TODO find a more suitable location for this module
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.xserver;
in {
  options.modules.desktop.xserver = {
    enable = mkEnableOption false;
  };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      videoDrivers = mkIf config.modules.hardware.nvidia.enable ["nvidia"];
      xkb = {
        options = "eurosign:e";
        layout = "us";
      };
    };

    environment.systemPackages = with pkgs; [
      feh
    ];

    user.packages = with pkgs; [
      gnome-screenshot
    ];
  };
}
