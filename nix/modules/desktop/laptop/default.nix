{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.desktop.laptop;
in {
  options.modules.desktop.laptop = { enable = mkEnableOption false; };

  config = mkIf cfg.enable {
    # TODO fill me out!
  };
}
