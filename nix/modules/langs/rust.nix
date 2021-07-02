{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.langs.rust;
in {
  options.modules.langs.rust = { enable = mkEnableOption true; };
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      cargo
      rustc
      rustfmt
      clippy
      rust-analyzer
      rls
      rustup
    ];
  };
}
