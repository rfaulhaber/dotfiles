{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.radicle;
in {
  options.modules.services.radicle = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      radicle-node
    ];
  };
}
