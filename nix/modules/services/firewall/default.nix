{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.services.firewall;
in {
  options.modules.services.firewall = { enable = mkEnableOption false; };

  config = mkIf cfg.enable {
    # TODO fill me out!
  };
}
