{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.systemd.modules.backupDockerConfig;
  description = "Backup Docker Config files.";
in {
  options.modules.services.systemd.modules.backupDockerConfig = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    systemd.services.backup-docker-config = {
      inherit description;
      path = with pkgs; [nushell zfs];
      after = ["multi-user.target"];
      wantedBy = ["multi-user.target"];
      serviceConfig = {
        Type = "oneshot";
        User = "root";
        ExecStart = let
          # convert it to a path to ensure it gets copied in a remote build
          # TODO I should have a better way of handling this...
          scriptPath = /. + "${config.dotfiles.binDir}/backup_zfs_dataset.nu";
        in "${pkgs.nushell}/bin/nu ${scriptPath} system/docker/config data/backup/docker_config";
      };
    };

    systemd.timers.backup-docker-config = {
      inherit description;

      wantedBy = ["timers.target"];

      timerConfig = {
        Persistent = true;
        OnCalendar = "daily";
      };
    };
  };
}
