{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.kubernetes;
in {
  options.modules.services.kubernetes = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    # TODO fill me out!
  };
}
