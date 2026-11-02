# Scheduled incremental Tumblr backups with archivr (github:rfaulhaber/archivr),
# one archivr-<job> service and timer per job.
#
# Blog names live in sops alongside the credentials, so this public config
# ties no job to a Tumblr account; job names are neutral for the same reason.
#
# Credentials are split by lifecycle. The OAuth consumer key/secret are stable
# and live in sops. The access/refresh token pair is not: archivr refreshes it
# mid-run and Tumblr rotates the refresh token on every renewal, so it lives
# only in /var/lib/archivr/auth.json, where a copy in sops would go stale after
# the first refresh. Seed it once by moving in an auth.json from an earlier
# interactive run, or by running archivr with --headless as `user` with
# XDG_DATA_HOME=/var/lib. A moved-in token must not stay in use at its old
# location: whichever copy refreshes first invalidates the other.
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib) mkIf mkEnableOption mkOption types optional optionals optionalAttrs escapeShellArgs literalExpression mapAttrs' mapAttrsToList nameValuePair listToAttrs unique;
  cfg = config.modules.services.archivr;
  stateDir = "/var/lib/archivr";
  envTemplate = name: "archivr-${name}-env";

  jobDir = name: "${cfg.archiveRoot}/${name}";

  # Paths are passed ahead of "$@" so a flag given on the command line, such
  # as --output for a scratch build, overrides the configured one.
  archivr-index = let
    indexer =
      pkgs.writers.writePython3Bin "archivr-index-unwrapped" {}
      (builtins.readFile "${config.dotfiles.binDir}/archivr-index.py");
    args =
      ["--json-dir" (jobDir cfg.search.jsonJob) "--output" cfg.search.database]
      ++ optionals (cfg.search.htmlJob != null) ["--html-dir" (jobDir cfg.search.htmlJob)];
  in
    pkgs.writeShellScriptBin "archivr-index" ''
      exec ${indexer}/bin/archivr-index-unwrapped ${escapeShellArgs args} "$@"
    '';

  archivr-search = lib.my.writeNushellScriptBin pkgs "archivr-search" (
    ''
      $env.PATH = ($env.PATH | prepend "${pkgs.sqlite}/bin")
      $env.ARCHIVR_DB = ($env.ARCHIVR_DB? | default "${cfg.search.database}")
    ''
    + builtins.readFile "${config.dotfiles.binDir}/archivr-search.nu"
  );

  jobArgs = name: job:
    [
      "--output-dir"
      "${cfg.archiveRoot}/${name}"
      # --resume first finishes an interrupted job (a first full backup spans
      # several days of rate-limit quota); without one it is a no-op and the
      # run is a plain incremental.
      "--incremental"
      "--resume"
      # No terminal to answer the OAuth prompt: with stdin at /dev/null a
      # missing or unrefreshable token fails the run immediately, instead of
      # holding port 6263 open for archivr's five-minute callback wait.
      "--headless"
    ]
    ++ job.extraArgs;

  mkService = name: job: {
    description = "Incremental Tumblr backup (archivr job ${name})";
    wants = ["network-online.target"];
    after = ["network-online.target"];
    path = optional (builtins.elem "--save-video-external" job.extraArgs) pkgs.yt-dlp;
    # archivr keeps its token at $XDG_DATA_HOME/archivr/auth.json.
    environment.XDG_DATA_HOME = "/var/lib";
    # archivr treats a missing state file as a first run, so writing into the
    # bare directory under an unmounted dataset would start a silent full
    # re-download onto the parent filesystem.
    unitConfig = optionalAttrs (cfg.mountPoint != null) {
      AssertPathIsMountPoint = cfg.mountPoint;
    };
    serviceConfig = {
      Type = "oneshot";
      User = cfg.user;
      Group = cfg.group;
      EnvironmentFile = config.sops.templates.${envTemplate name}.path;
      # Jobs share one token, and a refresh in one process invalidates the
      # refresh token another is holding, so runs take turns. They share the
      # consumer key's rate limit too, so overlapping would gain nothing.
      # systemd substitutes the blog name from the EnvironmentFile; left
      # unquoted it always expands to exactly one argument, and an unset
      # variable becomes an empty one that archivr rejects.
      ExecStart = "${pkgs.util-linux}/bin/flock ${stateDir}/session.lock ${cfg.package}/bin/archivr \${ARCHIVR_JOB_BLOG} ${escapeShellArgs (jobArgs name job)}";
      # Exit 1 means the run completed but found nothing new to archive,
      # the normal outcome of most incremental runs.
      SuccessExitStatus = 1;
      TimeoutStartSec = cfg.timeout;
      StateDirectory = "archivr";
      StateDirectoryMode = "0700";
      ProtectSystem = "strict";
      ReadWritePaths = [cfg.archiveRoot];
      ProtectHome = true;
      PrivateTmp = true;
      PrivateDevices = true;
      NoNewPrivileges = true;
      ProtectKernelTunables = true;
      ProtectKernelModules = true;
      ProtectControlGroups = true;
      RestrictAddressFamilies = ["AF_INET" "AF_INET6" "AF_UNIX"];
    };
  };

  mkTimer = name: _job: {
    description = "Scheduled incremental Tumblr backup (archivr job ${name})";
    wantedBy = ["timers.target"];
    timerConfig = {
      OnCalendar = cfg.timer.onCalendar;
      Persistent = true;
      RandomizedDelaySec = cfg.timer.randomizedDelaySec;
    };
  };

  forJobs = f: mapAttrs' (name: job: nameValuePair "archivr-${name}" (f name job)) cfg.jobs;
in {
  options.modules.services.archivr = {
    enable = mkEnableOption "scheduled incremental Tumblr backups with archivr";

    package = mkOption {
      description = "archivr package to run.";
      type = types.package;
      default = inputs.archivr.packages.${pkgs.stdenv.hostPlatform.system}.default;
      defaultText = literalExpression "inputs.archivr.packages.\${system}.default";
    };

    archiveRoot = mkOption {
      description = "Directory holding one archive per job, at <archiveRoot>/<job name>. Must already exist.";
      type = types.str;
      example = "/data/archive/tumblr/archivr";
    };

    mountPoint = mkOption {
      description = "Filesystem archiveRoot lives on; when set, each run first asserts it is mounted.";
      type = types.nullOr types.str;
      default = null;
      example = "/data/archive/tumblr";
    };

    jobs = mkOption {
      description = ''
        Backup jobs, keyed by the archive directory name under archiveRoot.
        One blog can have several jobs, e.g. one HTML and one --json archive.
        Job names end up in unit names and paths, so they should not name the
        blog.
      '';
      type = types.attrsOf (types.submodule {
        options = {
          blogSecret = mkOption {
            description = "sops key holding the name of the blog to back up.";
            type = types.str;
            example = "archivr/blog";
          };

          extraArgs = mkOption {
            description = ''
              Extra archivr flags. Post-selection filters (--include-tags,
              --type, --no-reblogs, --only-reblogs) are recorded in the
              archive's incremental state, and a run whose filters differ from
              the recorded ones refuses to proceed, so change them only
              alongside a new job.
            '';
            type = types.listOf types.str;
            default = [];
            example = ["--save-images" "--save-notes" "--index-file"];
          };
        };
      });
    };

    user = mkOption {
      description = "User the backups run as; must own the existing archives under archiveRoot.";
      type = types.str;
      default = config.user.name;
    };

    group = mkOption {
      description = "Group the backups run as.";
      type = types.str;
      default = "users";
    };

    timeout = mkOption {
      description = ''
        TimeoutStartSec for one run, including time spent waiting for another
        job to finish. Type=oneshot counts the whole run as startup, and a
        full backup fetches until the day's rate-limit quota runs out.
      '';
      type = types.str;
      default = "12h";
    };

    search = {
      enable = mkEnableOption ''
        the archivr-index and archivr-search commands. archivr-index builds a
        full-text-searchable SQLite database from a --json job's archive; it
        is run by hand and the database is not refreshed by the backup timers
      '';

      jsonJob = mkOption {
        description = "Job whose --json archive is indexed.";
        type = types.str;
        example = "json";
      };

      htmlJob = mkOption {
        description = "Job holding the matching HTML archive; indexed posts link to their page there.";
        type = types.nullOr types.str;
        default = null;
        example = "html";
      };

      database = mkOption {
        description = "Database archivr-index writes and archivr-search reads.";
        type = types.str;
        default = "${cfg.archiveRoot}/posts.db";
        defaultText = literalExpression ''"''${archiveRoot}/posts.db"'';
      };
    };

    timer = {
      onCalendar = mkOption {
        description = "systemd OnCalendar expression shared by every job.";
        type = types.str;
        default = "daily";
      };

      randomizedDelaySec = mkOption {
        description = "Jitter applied to each job's scheduled start.";
        type = types.str;
        default = "1h";
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.jobs != {};
        message = "modules.services.archivr.jobs must not be empty.";
      }
      {
        assertion = !cfg.search.enable || builtins.elem "--json" (cfg.jobs.${cfg.search.jsonJob}.extraArgs or []);
        message = "modules.services.archivr.search.jsonJob must name a job run with --json.";
      }
      {
        assertion = !cfg.search.enable || cfg.search.htmlJob == null || cfg.jobs ? ${cfg.search.htmlJob};
        message = "modules.services.archivr.search.htmlJob must name a configured job.";
      }
    ];

    sops.secrets =
      {
        "archivr/consumer-key" = {};
        "archivr/consumer-secret" = {};
      }
      // listToAttrs (map (s: nameValuePair s {}) (unique (mapAttrsToList (_: job: job.blogSecret) cfg.jobs)));

    sops.templates = mapAttrs' (name: job:
      nameValuePair (envTemplate name) {
        content = ''
          ARCHIVR_CONSUMER_KEY=${config.sops.placeholder."archivr/consumer-key"}
          ARCHIVR_CONSUMER_SECRET=${config.sops.placeholder."archivr/consumer-secret"}
          ARCHIVR_JOB_BLOG=${config.sops.placeholder.${job.blogSecret}}
        '';
      })
    cfg.jobs;

    # StateDirectory= only creates the directory when a job starts; this makes
    # it exist from activation, so a token can be moved in before the first run.
    systemd.tmpfiles.rules = ["d ${stateDir} 0700 ${cfg.user} ${cfg.group} -"];

    # On PATH for seeding the token and for ad-hoc runs.
    environment.systemPackages = [cfg.package] ++ optionals cfg.search.enable [archivr-index archivr-search];

    systemd.services = forJobs mkService;
    systemd.timers = forJobs mkTimer;
  };
}
