{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.awww;
in {
  options.modules.desktop.awww = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.modules.desktop.environment.isWayland;
        message = "Must use awww with a wayland desktop";
      }
    ];
    systemd.user.services.awww = {
      path = [pkgs.awww];
      wantedBy = ["graphical-session.target"];
      after = ["graphical-session.target"];
      wants = ["graphical-session.target"];
      environment = {
        WAYLAND_DISPLAY = "wayland-1";
      };
      serviceConfig = {
        ExecStart = "${pkgs.awww}/bin/awww-daemon";
        ExecStop = "${pkgs.awww}/bin/awww kill";
      };
    };
  };
}
