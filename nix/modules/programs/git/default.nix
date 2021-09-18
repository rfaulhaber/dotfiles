{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.programs.git;
in {
  options.modules.programs.git = {
    enable = mkEnableOption false;
    useFull = mkOption {
      type = types.bool;
      default = false;
      description = "Set to true to allow use of the full Git installation.";
      example = false;
    };
  };

  config = mkIf cfg.enable {
    home.programs.git = {
      enable = true;
      package = mkIf cfg.useFull pkgs.gitAndTools.gitFull;
      userName = config.userInfo.fullName;
      userEmail = config.userInfo.primaryEmail;

      signing = {
        signByDefault = true;
        key = "A2205925F3B6C5B96F26C3CB544650C5A306061B";
      };
    };
  };
}
