{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.zfs;
in {
  options.modules.services.zfs = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    services.zfs = {
      autoScrub.enable = true;
      trim.enable = true;
      autoSnapshot.enable = true;
    };
  };
}
