# inspired by:
# https://github.com/nix-community/home-manager/blob/master/modules/services/random-background.nix
#
# but reworked to use my own shell script instead
{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkIf mkOption optional optionalAttrs types escapeShellArgs;
  inherit (config.modules.desktop.environment) isX11 isWayland;
  cfg = config.modules.desktop.random-wallpaper;
  description = "Sets wallpaper using random-wallpaper script.";
in {
  options.modules.desktop.random-wallpaper =
    lib.my.wallpaperCommonOptions
    // {
      interval = mkOption {
        type = types.str;
        description = "Interval used for systemd unit (systemd timespan, e.g. \"30m\").";
        default = "30m";
      };
    };

  config = mkIf cfg.enable {
    systemd.user.services.random-wallpaper = let
      scriptPath =
        builtins.readFile "${config.dotfiles.binDir}/random-wallpaper.nu"
        |> lib.my.writeNushellScriptBin pkgs "random-wallpaper";
      desktop =
        if isWayland
        then "wayland"
        else "xserver";
      perDisplayArgs = optional cfg.perDisplay "--per-display";
      queryArgs = optional (cfg.query != "") cfg.query;
      args =
        ["--desktop" desktop]
        ++ perDisplayArgs
        ++ lib.my.wallpaperTokenArgs cfg.token
        ++ queryArgs;
    in {
      inherit description;
      path = with pkgs;
        [nushell]
        ++ optional isWayland awww
        ++ optional isX11 feh
        ++ optional (isX11 && cfg.perDisplay) xorg.xrandr;
      after = ["graphical-session.target"] ++ optional isWayland "awww.service" ++ optional config.modules.desktop.environment.niri.enable "niri.service";
      partOf = ["graphical-session.target"];
      wantedBy = ["graphical-session.target"];
      requires = optional isWayland "awww.service";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${scriptPath}/bin/random-wallpaper ${escapeShellArgs args}";
        IOSchedulingClass = "idle";
      };
      environment = optionalAttrs isWayland {
        WAYLAND_DISPLAY = "wayland-1";
      };
    };

    systemd.user.timers.random-wallpaper = {
      inherit description;
      wantedBy = ["timers.target"];
      timerConfig.OnUnitActiveSec = cfg.interval;
    };
  };
}
