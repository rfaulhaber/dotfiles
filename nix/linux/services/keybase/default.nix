{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.services.keybase;
in {
  options.modules.services.keybase = { enable = mkEnableOption false; };

  config = mkIf cfg.enable {
    services = {
      keybase.enable = true;
      kbfs.enable = true;
    };

    environment.systemPackages = with pkgs; [ keybase keybase-gui kbfs ];
  };
}
