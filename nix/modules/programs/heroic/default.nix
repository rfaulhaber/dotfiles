{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.heroic;
in {
  options.modules.programs.heroic = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    programs = {
      gamescope.enable = true;
      gamemode.enable = true;
    };

    user.packages = [pkgs.heroic];
  };
}
