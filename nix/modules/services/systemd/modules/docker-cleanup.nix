{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.systemd.modules.dockerCleanup;
  description = "Cleanup Docker.";
in {
  options.modules.services.systemd.modules.dockerCleanup = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    systemd.services.dockerCleanup = {
      inherit description;
      path = with pkgs; [docker coreutils bash];
      after = ["multi-user.target"];
      wantedBy = ["multi-user.target"];
      serviceConfig = {
        Type = "oneshot";
        User = "root";
        ExecStart = let
          dockerExec = "${pkgs.docker}/bin/docker";
          systemPruneCmd = "${dockerExec} system prune --volumes -a";
          yesExec = "${pkgs.coreutils}/bin/yes";
        in "${pkgs.bash}/bin/bash -c '(${yesExec} | ${systemPruneCmd})'";
      };
    };

    systemd.timers.dockerCleanup = {
      inherit description;

      wantedBy = ["timers.target"];

      timerConfig = {
        Persistent = true;
        OnCalendar = "weekly";
      };
    };
  };
}
