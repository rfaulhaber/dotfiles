{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.modules.services.systemd;
  description = "Cleans up Docker.";
in {
  config = mkIf (cfg.enable && (builtins.elem "docker-cleanup" cfg.modules)) {
    systemd.services.docker-cleanup = {
      inherit description;
      path = with pkgs; [ docker coreutils bash ];
      after = [ "multi-user.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        User = "root";
        ExecStart = let
          dockerExec = "${pkgs.docker}/bin/docker";
          systemPruneCmd = "${dockerExec} system prune -a";
          volumePruneCmd = "${dockerExec} volume prune";
          yesExec = "${pkgs.coreutils}/bin/yes";
        in "${pkgs.bash}/bin/bash -c '(${yesExec} | ${systemPruneCmd}) && (${yesExec} | ${volumePruneCmd})'";
      };
    };

    systemd.timers.docker-cleanup = {
      inherit description;

      wantedBy = [ "timers.target" ];

      timerConfig = {
        Persistent = true;
        OnCalendar = "weekly";
      };
    };
  };
}
