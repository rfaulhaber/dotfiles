{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.environment.niri;
in {
  imports = [
    ../../swww
    ../../wayland
  ];

  options.modules.desktop.environment.niri = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    nix.settings = {
      substituters = ["https://niri.cachix.org"];
      trusted-public-keys = ["niri.cachix.org-1:Wv0OmO7PsuocRKzfDoJ3mulSl7Z6oezYhGhR+3W2964="];
    };

    modules = {
      desktop = {
        swww.enable = true;
        wayland.enable = true;
        environment.type = "wayland";
      };
    };

    programs = {
      niri = {
        enable = true;
        package = inputs.niri.packages.${pkgs.stdenv.hostPlatform.system}.default;
      };

      uwsm.waylandCompositors.niri = {
        prettyName = "niri";
        comment = "niri compositor for UWSM.";
        binPath = "/run/current-system/sw/bin/niri";
      };
    };
  };
}
