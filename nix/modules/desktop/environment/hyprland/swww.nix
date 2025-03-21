# TODO move
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.environment.hyprland.swww;
in {
  options.modules.desktop.environment.hyprland.swww = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    systemd.user.services.swww = let
      swwwPkg = inputs.swww.packages.${pkgs.system}.swww;
    in {
      path = [swwwPkg];
      wantedBy = ["random-wallpaper.service"];
      serviceConfig = {
        ExecStart = "${swwwPkg}/bin/swww-daemon";
        ExecStop = "${swwwPkg}/bin/swww kill";
      };
    };
  };
}
