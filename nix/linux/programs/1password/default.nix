{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.programs._1password;
in {
  options.modules.programs._1password = { enable = mkEnableOption false; };
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ _1password _1password-gui ];
  };
}
