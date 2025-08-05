{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.aspell;
in {
  options.modules.programs.aspell = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      (aspellWithDicts
        (dicts: with dicts; [en en-computers en-science]))
    ];
  };
}
