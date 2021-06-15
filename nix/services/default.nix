{ config, lib, pkgs, ... }:

with lib;
with lib.my;

let cfg = config.services;
in {
  imports = [ ./bspwm ./polybar ];
  options.services = { polybar.enable = mkBoolOpt false; };
}
