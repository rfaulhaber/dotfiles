{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.containers;
in {
  imports = [
    ./mullvad.nix
  ];

  options.modules.services.containers = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    boot.enableContainers = true;
  };
}
