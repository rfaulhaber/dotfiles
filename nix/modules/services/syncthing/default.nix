{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.services.syncthing;
in {
  options.modules.services.syncthing = {
    enable = mkEnableOption false;
    useTray = mkOption {
      type = types.bool;
      default = false;
      description = "If true, installs tray utility.";
    };
  };

  config = mkIf cfg.enable {
    home.services.syncthing = {
      enable = true;
      tray = mkIf cfg.useTray { enable = true; };
    };
  };
}
