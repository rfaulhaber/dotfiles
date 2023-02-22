{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.systemd;
in {
  imports = [./updatedb.nix ./docker-cleanup.nix];
  options.modules.services.systemd = {
    enable = mkEnableOption false;
    modules = mkOption {
      description = "List of extra systemd modules to enable.";
      type = types.listOf types.str;
    };
  };
}
