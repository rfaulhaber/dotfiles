{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.hardware.zsa;
in {
  options.modules.hardware.zsa = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    hardware.keyboard.zsa.enable = true;
    environment.systemPackages = with pkgs; [keymapp];
  };
}
