{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.services.docker;
in {
  options.modules.services.docker = { enable = mkEnableOption false; };

  config = mkIf cfg.enable {
    virtualisation.docker.enable = true;

    environment.systemPackages = with pkgs; [ docker docker-compose ];

    # TODO make more dynamic than this
    user.extraGroups = [ "docker" ];
  };
}
