{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.langs.racket;
in {
  options.modules.langs.racket = { enable = mkEnableOption false; };

  config =
    mkIf cfg.enable { environment.systemPackages = with pkgs; [ racket ]; };
}
