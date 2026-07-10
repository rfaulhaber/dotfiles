# Watched-directory Bandcamp importer: a path unit watches
# <watchDir>/incoming for *.zip and triggers a oneshot service running
# bin/bandcamp-import.nu in batch mode. PathExistsGlob is level-triggered —
# systemd re-checks it whenever the service deactivates — so draining a batch
# needs no loop here; the script guarantees every zip leaves incoming/ before
# a clean exit (see its header for the full contract).
{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkIf mkEnableOption mkOption types optional optionalAttrs escapeShellArgs;
  cfg = config.modules.services.bandcamp-import;
in {
  options.modules.services.bandcamp-import = {
    enable = mkEnableOption "watched-directory Bandcamp album importer";

    watchDir = mkOption {
      description = "Drop directory root; incoming/, archive/ and failed/ live beneath it.";
      type = types.str;
      default = "/data/import/bandcamp";
    };

    musicRoot = mkOption {
      description = "Library root albums are placed into as <root>/<Artist>/<Album>.";
      type = types.str;
      default = "/data/music";
    };

    user = mkOption {
      description = ''
        User the importer runs as. Must match the uid that owns the music
        library (the lidarr/navidrome containers run as uid 1000) so imported
        files are indistinguishable from Lidarr-placed ones.
      '';
      type = types.str;
      default = config.user.name;
    };

    group = mkOption {
      description = "Group the importer runs as.";
      type = types.str;
      default = "users";
    };

    settleSeconds = mkOption {
      description = ''
        Only import a zip once its mtime is at least this old. scp writes
        in-place under the final name, so the watch glob matches while the
        upload is still streaming; this gate prevents importing half a file.
      '';
      type = types.int;
      default = 60;
    };

    archiveDays = mkOption {
      description = "Prune imported zips from archive/ after this many days.";
      type = types.int;
      default = 30;
    };

    failedDays = mkOption {
      description = "Prune quarantined zips from failed/ after this many days.";
      type = types.int;
      default = 90;
    };

    zfsDataset = mkOption {
      description = ''
        Optional dataset registered via modules.services.zfs.datasets and
        mounted at watchDir. Null leaves watchDir on whatever filesystem
        already backs it.
      '';
      type = types.nullOr types.str;
      default = null;
      example = "data/import/bandcamp";
    };

    lidarr = {
      enable = mkOption {
        description = "Register imported artists in Lidarr (unmonitored) after placement.";
        type = types.bool;
        default = true;
      };
      url = mkOption {
        description = "Lidarr API base URL reachable from the host.";
        type = types.str;
        default = "http://127.0.0.1:8686";
      };
      configXml = mkOption {
        description = "Lidarr config.xml holding the API key; must be readable by `user`.";
        type = types.str;
        default = "/data/apps/lidarr/config.xml";
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.zfsDataset == null || config.modules.services.zfs.enable;
        message = "modules.services.bandcamp-import.zfsDataset requires modules.services.zfs.enable.";
      }
    ];

    modules.services.zfs.datasets = optionalAttrs (cfg.zfsDataset != null) {
      ${cfg.zfsDataset}.properties.mountpoint = cfg.watchDir;
    };

    # tmpfiles owns directory setup: `d` re-applies ownership at boot and on
    # every activation (a freshly created dataset mounts root-owned), and the
    # age field prunes archive/ and failed/ so zips don't accumulate forever.
    systemd.tmpfiles.rules = [
      "d ${cfg.watchDir} 0755 ${cfg.user} ${cfg.group} -"
      "d ${cfg.watchDir}/incoming 0755 ${cfg.user} ${cfg.group} -"
      "d ${cfg.watchDir}/archive 0755 ${cfg.user} ${cfg.group} ${toString cfg.archiveDays}d"
      "d ${cfg.watchDir}/failed 0700 ${cfg.user} ${cfg.group} ${toString cfg.failedDays}d"
    ];

    systemd.paths.bandcamp-import = {
      description = "Watch ${cfg.watchDir}/incoming for Bandcamp zips";
      wantedBy = ["multi-user.target"];
      # An inotify watch established before the dataset mounts would sit on
      # the shadowed mountpoint inode and never fire (mounting generates no
      # inotify event), so only start watching once declarative datasets exist.
      after = ["zfs-manage-datasets.service"];
      pathConfig.PathExistsGlob = "${cfg.watchDir}/incoming/*.zip";
    };

    systemd.services.bandcamp-import = let
      script =
        builtins.readFile "${config.dotfiles.binDir}/bandcamp-import.nu"
        |> lib.my.writeNushellScriptBin pkgs "bandcamp-import";
      args =
        [cfg.watchDir "--music-root" cfg.musicRoot "--settle-seconds" (toString cfg.settleSeconds)]
        ++ (
          if cfg.lidarr.enable
          then ["--lidarr-url" cfg.lidarr.url "--lidarr-config" cfg.lidarr.configXml]
          else ["--no-lidarr"]
        );
    in {
      description = "Import Bandcamp zips from ${cfg.watchDir}/incoming";
      # Ordering only — an import while Lidarr is down degrades to
      # Navidrome-only placement and logs it, so the container is not a hard
      # dependency.
      after = ["zfs-manage-datasets.service"] ++ optional cfg.lidarr.enable "podman-lidarr.service";
      path = with pkgs; [unzip rsync ffmpeg coreutils];
      serviceConfig = {
        Type = "oneshot";
        User = cfg.user;
        Group = cfg.group;
        ExecStart = "${script}/bin/bandcamp-import batch ${escapeShellArgs args}";
        # Type=oneshot counts the whole run as startup; the 90s default would
        # kill a large batch mid-transfer.
        TimeoutStartSec = "2h";
        PrivateTmp = true;
        NoNewPrivileges = true;
      };
    };
  };
}
