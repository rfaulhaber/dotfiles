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
    # does this even need to be a module?
    user.packages = with pkgs; [
      heroic
    ];
  };
}
