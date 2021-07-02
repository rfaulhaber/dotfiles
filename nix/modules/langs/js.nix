{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.langs.js;
in {
  options.modules.langs.js = { enable = mkEnableOption true; };
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      nodejs
      yarn
      nodePackages.prettier
      nodePackages.javascript-typescript-langserver
      nodePackages.typescript
    ];
  };
}
