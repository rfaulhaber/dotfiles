{
  config,
  lib,
  pkgs,
  ...
}:
# NOTE: might be worth looking into some of the advanced Nix options:
# https://search.nixos.org/options?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=syncthing
with lib; let
  cfg = config.modules.services.syncthing;
in {
  options.modules.services.syncthing = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {home.services.syncthing.enable = true;};
}
