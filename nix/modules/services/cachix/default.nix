{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.services.cachix;
in {
  options.modules.services.cachix = { enable = mkEnableOption false; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ cachix ];
  };
}
