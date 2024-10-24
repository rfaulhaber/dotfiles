{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.desktop.environment.retroarch;
in {
  options.modules.desktop.environment.retroarch = { enable = mkEnableOption false; };

  config = mkIf cfg.enable {
    modules.desktop = {
      xserver.enable = true;
      environment.type = "x11";
    };

    services.xserver.desktopManager.retroarch = {
      enable = true;
    };
  };
}
