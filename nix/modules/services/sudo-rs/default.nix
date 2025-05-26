{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.services.sudo-rs;
in {
  options.modules.services.sudo-rs = { enable = mkEnableOption false; };

  config = mkIf cfg.enable {
    security = {
      sudo-rs.enable = true;
      sudo.enable = false;
    };
  };
}
