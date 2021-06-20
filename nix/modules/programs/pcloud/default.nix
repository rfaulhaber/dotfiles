{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.modules.programs.pcloud;
  pcloud = import ./pcloud.nix pkgs;
in {
  options.modules.programs.pcloud = { enable = mkEnableOption false; };

  config = mkIf cfg.enable { environment.systemPackages = [ pcloud ]; };
}
