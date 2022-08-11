{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.programs._1password;
in {
  options.modules.programs._1password = { enable = mkEnableOption false; };
  config = mkIf cfg.enable {
    programs = {
      _1password = {
        enable = true;
        package = pkgs._1password;
      };

      _1password-gui = {
        enable = true;
        package = pkgs._1password-gui;
        polkitPolicyOwners = [ config.user.name ];
      };
    };

  };
}
