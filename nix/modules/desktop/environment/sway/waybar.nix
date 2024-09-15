{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.environment.sway.waybar;
in {
  options.modules.environment.sway.waybar = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    # TODO fill me out!
  };
}
