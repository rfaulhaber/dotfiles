{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.services.zerotier;
in {
  options.modules.services.zerotier = {
    enable = mkEnableOption false;
    networks = mkOption {
      description = "List of networks to join.";
      type = types.listOf types.str;
      default = [ ];
    };
    port = mkOption {
      description = "Port to be used by ZeroTier";
      type = types.int;
      default = 9993;
    };
  };

  config = mkIf cfg.enable {
    services.zerotierone = {
      enable = true;
      joinNetworks = cfg.networks;
      port = cfg.port;
    };
  };
}
