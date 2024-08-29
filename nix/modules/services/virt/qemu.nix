{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.virt.qemu;
in {
  options.modules.services.virt.qemu = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    # TODO fill me out!
  };
}
