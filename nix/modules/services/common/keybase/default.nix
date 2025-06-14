{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.keybase;
in {
  options.modules.services.keybase = {enable = mkEnableOption false;};

  config = mkIf cfg.enable (mkMerge [
    # Cross-platform packages
    {
      environment.systemPackages = with pkgs; [keybase keybase-gui kbfs];
    }

    # Linux-specific services temporarily disabled for Darwin compatibility debugging
    # (mkIf pkgs.stdenv.isLinux {
    #   services = {
    #     keybase.enable = true;
    #     kbfs.enable = true;
    #   };
    # })
  ]);
}
