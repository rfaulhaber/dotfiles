{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.services.virt.lxc;
in {
  options.modules.services.virt.lxc = { enable = mkEnableOption false; };

  config = mkIf cfg.enable {
    # TODO fill me out!
  };
}
