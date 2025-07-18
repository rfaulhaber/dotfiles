{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.ssh;
in {
  imports = [
    ./client.nix
    ./server.nix
  ];
  options.modules.services.ssh = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    # TODO fill me out!
  };
}
