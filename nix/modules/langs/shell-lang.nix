{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.langs.shell;
in {
  options.modules.langs.shell = { enable = mkEnableOption false; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ shellcheck shfmt ];
  };
}
