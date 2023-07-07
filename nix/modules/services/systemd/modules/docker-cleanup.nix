{
  lib,
  pkgs,
  ...
}:
with lib; let
  description = "Cleans up Docker.";
in {
  name = "dockerCleanup";
  service = {
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

  timer = {
    inherit description;

    wantedBy = ["timers.target"];

    timerConfig = {
      Persistent = true;
      OnCalendar = "weekly";
    };
  };
}
