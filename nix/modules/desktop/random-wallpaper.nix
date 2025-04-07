# inspired by:
# https://github.com/nix-community/home-manager/blob/master/modules/services/random-background.nix
#
# but reworked to use my own shell script instead
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  inherit (config.modules.desktop.environment) isX11 isWayland;
  cfg = config.modules.desktop.random-wallpaper;
  description = "Sets wallpaper using random-wallpaper script.";
in {
  options.modules.desktop.random-wallpaper = {
    enable = mkEnableOption false;

    interval = mkOption {
      type = types.str;
      description = "Interval used for systemd unit.";
      default = "30m";
    };

    query = mkOption {
      type = types.str;
      description = "Optional query to pass to random wallpaper endpoint";
      default = "";
    };

    token = mkOption {
      description = "API token for wallpaper API.";
      type = types.either types.str types.path;
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # TODO random-wallpaper should be a nix module
      systemd.user.services.random-wallpaper = let
        scriptPath = "${config.dotfiles.binDir}/random-wallpaper.nu";
        nuExec = "${pkgs.nushell}/bin/nu";
        desktop =
          if config.modules.desktop.environment.hyprland.enable
          then "hyprland"
          else if isWayland
          then "wayland"
          else "xserver";
        exec = "${nuExec} -c '${scriptPath} --desktop ${desktop} --token (open ${cfg.token}) ${cfg.query}'";
      in {
        inherit description;
        path = with pkgs;
          [nushell]
          ++ lib.optional isWayland inputs.swww.packages.${pkgs.stdenv.hostPlatform.system}.swww
          ++ lib.optional isX11 feh
          ++ lib.optional (desktop == "hyprland") inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
        after = ["graphical-session-pre.target" "network-online.target"] ++ lib.optional isWayland "swww.service";
        partOf = ["graphical-session.target"];
        wantedBy = ["graphical-session.target"];
        requires = lib.optional isWayland "swww.service";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${exec}";
          IOSchedulingClass = "idle";
        };
        environment = lib.optionalAttrs isWayland {
          WAYLAND_DISPLAY = "wayland-1";
        };
      };
    }
    (mkIf (cfg.interval != null) {
      systemd.user.timers.random-wallpaper = {
        inherit description;

        wantedBy = ["timers.target"];

        timerConfig = {OnUnitActiveSec = cfg.interval;};
      };
    })
  ]);
}
