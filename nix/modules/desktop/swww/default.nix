{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.swww;
in {
  options.modules.desktop.swww = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.modules.desktop.environment.isWayland;
        message = "Must use swww with a wayland desktop";
      }
    ];
    systemd.user.services.swww = let
      swwwPkg = inputs.swww.packages.${pkgs.system}.swww;
    in {
      path = [swwwPkg];
      wantedBy = ["random-wallpaper.service"];
      environment = {
        WAYLAND_DISPLAY = "wayland-1";
      };
      serviceConfig = {
        ExecStart = "${swwwPkg}/bin/swww-daemon";
        ExecStop = "${swwwPkg}/bin/swww kill";
      };
    };
  };
}
