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
  inherit (lib) mkIf mkOption optional optionals optionalAttrs types escapeShellArgs;
  inherit (config.modules.desktop.environment) isX11 isWayland;
  cfg = config.modules.desktop.random-wallpaper;
  noctaliaCfg = config.modules.desktop.noctalia;
  # noctalia paints its own background layer, so once it owns the wallpaper
  # awww is gone and the script hands images to noctalia over IPC instead.
  useNoctalia = isWayland && noctaliaCfg.enable && noctaliaCfg.wallpaper.enable;
  desktop =
    if useNoctalia
    then "noctalia"
    else if isWayland
    then "wayland"
    else "xserver";
  description = "Sets wallpaper using random-wallpaper script.";

  scriptPath =
    builtins.readFile "${config.dotfiles.binDir}/random-wallpaper.nu"
    |> lib.my.writeNushellScriptBin pkgs "random-wallpaper";
  baseFlags =
    ["--desktop" desktop]
    ++ optionals useNoctalia ["--store-dir" cfg.storeDir]
    ++ lib.my.wallpaperTokenArgs cfg.token;
  queryArgs = optional (cfg.query != "") cfg.query;

  mkUnit = extraFlags: {
    inherit description;
    path =
      [pkgs.nushell]
      ++ optional (isWayland && !useNoctalia) pkgs.awww
      ++ optionals useNoctalia [pkgs.noctalia pkgs.niri]
      ++ optional isX11 pkgs.feh
      ++ optional (isX11 && cfg.perDisplay) pkgs.xorg.xrandr;
    after =
      ["graphical-session.target"]
      ++ optional (isWayland && !useNoctalia) "awww.service"
      ++ optional useNoctalia "noctalia.service"
      ++ optional config.modules.desktop.environment.niri.enable "niri.service";
    partOf = ["graphical-session.target"];
    requires =
      optional (isWayland && !useNoctalia) "awww.service"
      ++ optional useNoctalia "noctalia.service";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${scriptPath}/bin/random-wallpaper ${escapeShellArgs (baseFlags ++ extraFlags ++ queryArgs)}";
      IOSchedulingClass = "idle";
    };
    environment = optionalAttrs isWayland {
      WAYLAND_DISPLAY = "wayland-1";
    };
  };
in {
  options.modules.desktop.random-wallpaper =
    lib.my.wallpaperCommonOptions
    // {
      interval = mkOption {
        type = types.str;
        description = "Interval used for systemd unit (systemd timespan, e.g. \"30m\").";
        default = "30m";
      };

      storeDir = mkOption {
        type = types.str;
        default = "${config.user.home}/.local/share/random-wallpaper/wallpapers";
        description = ''
          Where downloads are kept when noctalia renders the wallpaper. noctalia
          re-opens its persisted paths at startup, so unlike the temp dir the
          other backends use this has to survive a reboot. Pruned by the script.
        '';
      };
    };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = !useNoctalia || config.modules.desktop.environment.niri.enable;
        message = "random-wallpaper enumerates outputs through niri when noctalia renders the wallpaper";
      }
    ];

    systemd.user.services =
      {
        random-wallpaper =
          mkUnit (optional cfg.perDisplay "--per-display")
          // {
            wantedBy = ["graphical-session.target"];
          };
      }
      // optionalAttrs useNoctalia {
        # One instance per connector, started on demand: the control-center
        # tile runs `systemctl --user start random-wallpaper@DP-1`. systemd
        # expands %i to the instance name.
        "random-wallpaper@" =
          mkUnit ["--output" "%i"]
          // {
            description = "${description} (one output)";
          };
      };

    systemd.user.timers.random-wallpaper = {
      inherit description;
      wantedBy = ["timers.target"];
      timerConfig.OnUnitActiveSec = cfg.interval;
    };
  };
}
