# this module is for common xserver settings for all xserver-based desktop environments

# TODO find a more suitable location for this module

{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.desktop.xserver;
  videoDrivers = config.modules.desktop.videoDrivers;
in {
  options.modules.desktop.xserver = {
    enable = mkEnableOption false;
  };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      xkb = {
        options = "eurosign:e";
        layout = "us";
      };
      videoDrivers = mkIf ((length videoDrivers) > 0) videoDrivers;
    };
  };
}
