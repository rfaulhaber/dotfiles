{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.services.astal;
in {
  options.modules.services.astal = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    environment.systemPackages = [inputs.astal.packages.${pkgs.system}.default];
  };
}
