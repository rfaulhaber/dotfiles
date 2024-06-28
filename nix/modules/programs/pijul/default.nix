{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.pijul;
in {
  options.modules.programs.pijul = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [pijul];
  };
}
