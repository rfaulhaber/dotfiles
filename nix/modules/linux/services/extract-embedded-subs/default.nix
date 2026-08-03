# Pre-extract embedded subtitle tracks so Jellyfin never has to.
#
# Jellyfin hands an external subtitle file straight to the client, but an
# embedded track goes through SubtitleEncoder.GetReadableFile, which only
# short-circuits when the stream is IsExternal. Everything else demuxes the
# whole container to collect a few KB of cues: Matroska interleaves subtitle
# blocks with video and audio in presentation order, so nothing indexes them
# without a linear read. When the media is served over NFS that read saturates
# the link and stalls playback start — a 10.4GB remux cost 90s of dead air at
# 1GbE line rate. Writing the sidecar here, on the host that owns the bytes,
# removes the penalty permanently.
#
# Probably temporary: https://github.com/jellyfin/jellyfin/issues/17499
{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkIf mkEnableOption mkOption types escapeShellArgs optionals;
  cfg = config.modules.services.extract-embedded-subs;

  # ffmpeg/ffprobe/find are baked into the wrapper rather than supplied by the
  # unit's `path`, so the command behaves identically however it is started —
  # from the timer, from an ad-hoc `systemd-run`, or straight from a shell.
  package = pkgs.symlinkJoin {
    name = "extract-embedded-subs";
    paths = [
      (builtins.readFile "${config.dotfiles.binDir}/extract-embedded-subs.nu"
        |> lib.my.writeNushellScriptBin pkgs "extract-embedded-subs")
    ];
    nativeBuildInputs = [pkgs.makeWrapper];
    postBuild = ''
      wrapProgram $out/bin/extract-embedded-subs \
        --prefix PATH : ${lib.makeBinPath (with pkgs; [ffmpeg findutils])}
    '';
  };

  args =
    cfg.mediaRoots
    ++ ["--title" cfg.title "--jobs" (toString cfg.jobs)]
    ++ optionals (cfg.limit > 0) ["--limit" (toString cfg.limit)];

  # Jellyfin's ExternalPathParser matches these against the filename's dot
  # tokens: the first group with Contains(), the second with Equals().
  flagInfixes = ["default" "forced" "foreign"];
  flagWords = ["cc" "hi" "sdh"];
  lowerTitle = lib.toLower cfg.title;
in {
  options.modules.services.extract-embedded-subs = {
    enable = mkEnableOption "embedded subtitle sidecar extraction";

    mediaRoots = mkOption {
      description = "Library roots to sweep for video files.";
      type = types.listOf types.str;
      default = ["/data/movies" "/data/tv"];
    };

    title = mkOption {
      description = ''
        Token baked into each sidecar filename, so a viewer can tell the
        sidecar from the embedded track in the player's subtitle picker.
        Jellyfin renders Title first and its own "External" tag last, and the
        trailing tag is what gets cut off when a TV client truncates the label
        — hence a leading marker rather than relying on "External".

        Values Jellyfin would read as a subtitle flag are rejected by an
        assertion below rather than silently flagging every sidecar.
      '';
      type = types.str;
      default = "Fast Start";
    };

    user = mkOption {
      description = ''
        User the sweep runs as. Must be able to write into the media
        directories, and should match whoever owns the existing files so the
        sidecars are indistinguishable from hand-placed ones.
      '';
      type = types.str;
      default = config.user.name;
    };

    group = mkOption {
      description = "Group the sweep runs as.";
      type = types.str;
      default = "users";
    };

    jobs = mkOption {
      description = ''
        Concurrent ffmpeg extractions. Each one is a whole-file sequential read
        with almost no CPU cost (`-c:s copy` is a pure demux), so this is a
        spindle question rather than a core-count one — the useful ceiling is
        roughly two streams per pool vdev.

        The default is deliberately low: the timer fires unattended and may
        overlap live playback off the same pool. Pass a higher --jobs on an
        ad-hoc run when nothing else is reading.
      '';
      type = types.ints.positive;
      default = 2;
    };

    limit = mkOption {
      description = ''
        Stop each run after this many extractions; 0 means unlimited. Every
        extraction is a full-file read, so an unbounded first sweep would pin
        the pool for an entire night — the backlog gets worked off a little at
        a time instead.
      '';
      type = types.ints.unsigned;
      default = 50;
    };

    timeout = mkOption {
      description = ''
        TimeoutStartSec for the sweep. Type=oneshot counts the whole run as
        startup, so the 90s default would kill it mid-sweep.
      '';
      type = types.str;
      default = "6h";
    };

    timer = {
      enable = mkOption {
        description = ''
          Run the sweep on a schedule. Disabling leaves the service and the
          packaged command in place for manual runs.
        '';
        type = types.bool;
        default = true;
      };

      onCalendar = mkOption {
        description = "systemd OnCalendar expression for the sweep.";
        type = types.str;
        default = "03:00";
      };

      randomizedDelaySec = mkOption {
        description = "Jitter applied to the scheduled start.";
        type = types.str;
        default = "30m";
      };
    };

    linger = mkOption {
      description = ''
        Enable systemd lingering for `user`. A transient `systemd-run --user`
        backfill is a child of that user's manager, which logind tears down
        once the last session ends — without lingering, a sweep kicked off over
        SSH dies at logout.
      '';
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.mediaRoots != [];
        message = "modules.services.extract-embedded-subs.mediaRoots must not be empty.";
      }
      {
        assertion = !(lib.any (bad: lib.hasInfix bad lowerTitle) flagInfixes);
        message = ''
          modules.services.extract-embedded-subs.title ("${cfg.title}") contains one of
          ${lib.concatStringsSep ", " flagInfixes}. Jellyfin's ExternalPathParser matches
          those with Contains(), so every sidecar written would carry that flag.
        '';
      }
      {
        assertion = !(builtins.elem lowerTitle flagWords);
        message = ''
          modules.services.extract-embedded-subs.title ("${cfg.title}") is read by Jellyfin
          as a hearing-impaired flag. Pick a title that is not one of
          ${lib.concatStringsSep ", " flagWords}.
        '';
      }
    ];

    # Exposed as a plain command so a one-off backfill can be launched over SSH
    # with `systemd-run` against /run/current-system/sw/bin, which stays stable
    # across generations, instead of a store path that does not.
    environment.systemPackages = [package];

    users.users = mkIf cfg.linger {
      ${cfg.user}.linger = true;
    };

    systemd.services.extract-embedded-subs = {
      description = "Extract embedded subtitle tracks to sidecar files";
      after = ["zfs-manage-datasets.service"];
      serviceConfig = {
        Type = "oneshot";
        User = cfg.user;
        Group = cfg.group;
        ExecStart = "${package}/bin/extract-embedded-subs ${escapeShellArgs args}";
        TimeoutStartSec = cfg.timeout;
        # Worth setting, but do not lean on it where the media lives on ZFS:
        # ZFS uses its own vdev queues rather than the kernel block-layer
        # scheduler, so the ioprio class barely registers. A low `jobs` is the
        # real protection against starving an in-flight playback read.
        IOSchedulingClass = "idle";
        Nice = 19;
        PrivateTmp = true;
        NoNewPrivileges = true;
      };
    };

    systemd.timers.extract-embedded-subs = mkIf cfg.timer.enable {
      description = "Scheduled embedded-subtitle sidecar sweep";
      wantedBy = ["timers.target"];
      timerConfig = {
        OnCalendar = cfg.timer.onCalendar;
        Persistent = true;
        RandomizedDelaySec = cfg.timer.randomizedDelaySec;
      };
    };
  };
}
