{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.programs.starship;
in {
  options.modules.programs.starship = { enable = mkEnableOption false; };

  config = mkIf cfg.enable {
    programs.starship = {
      enable = true;
      enableZshIntegration = mkIf config.modules.programs.zsh.enable true;
      settings = {
        add_newline = false;
        format = concatStrings [
          "$username"
          "@"
          "$hostname"
          "$line_break"
          "$package"
          "$line_break"
          "$character"
        ];
        scan_timeout = 10;
        character = {
          success_symbol = "[➜](bold green)";
          error_symbol = "[➜](bold red)";
        };
      };
    };
  };
}
