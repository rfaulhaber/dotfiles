{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.services.syncthing;
in {
  options.modules.services.syncthing = { enable = mkEnableOption false; };

  config = mkIf cfg.enable { services.syncthing = { enable = true; }; };
}
