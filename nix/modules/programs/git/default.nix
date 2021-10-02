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
    useDelta = mkOption {
      type = types.bool;
      default = false;
      description = "Use delta for git diffing.";
      example = false;
    };
  };

  config = mkIf cfg.enable {
    home.programs.git = {
      enable = true;
      package = mkIf cfg.useFull pkgs.gitAndTools.gitFull;
      userName = config.userInfo.fullName;
      userEmail = config.userInfo.primaryEmail;

      delta = mkIf cfg.useDelta {
        enable = true;
        options = { line-numbers = true; };
      };

      signing = {
        signByDefault = true;
        key = config.userInfo.primaryGPGKey;
      };

      extraConfig = { init.defaultBranch = "main"; };
    };
  };
}
