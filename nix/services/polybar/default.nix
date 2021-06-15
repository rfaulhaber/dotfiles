{ config, lib, pkgs, ... }:

with lib;
with lib.my;

let cfg = config.services.polybar;
in {
  options.services.polybar = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable { home.services.polybar = { enable = true; }; };
}
