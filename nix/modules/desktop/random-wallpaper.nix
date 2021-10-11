# inspired by:
# https://github.com/nix-community/home-manager/blob/master/modules/services/random-background.nix
#
# but reworked to use my own shell script instead

{ config, lib, pkgs, ... }:

with lib;

let
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
  };

  config = mkIf cfg.enable (mkMerge ([
    {
      assertions = [{
        assertion = config.services.xserver.enable;
        message = "Cannot use random-wallpaper without xserver.";
      }];
      systemd.user.services.random-wallpaper = {
        description = description;
        path = with pkgs; [ bash jq pass feh curl ];
        after = [ "graphical-session-pre.target" ];
        partOf = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${config.dotfiles.binDir}/random-wallpaper ${cfg.query}";
          IOSchedulingClass = "idle";
        };
      };
    }
    (mkIf (cfg.interval != null) {
      systemd.user.timers.random-wallpaper = {
        inherit description;

        wantedBy = [ "timers.target" ];

        timerConfig = { OnUnitActiveSec = cfg.interval; };
      };
    })
  ]));
}
