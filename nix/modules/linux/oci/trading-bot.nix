{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.trading-bot;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};

  settingsFormat = pkgs.formats.toml {};

  # The news commands only read [news]/[news.llm], but the CLI parses the
  # whole config file, so the remaining sections carry valid inert values.
  # Store paths resolve relative to the config file, which is bind-mounted
  # into baseDir at /data/config.toml — so "news" means <baseDir>/news.
  defaultSettings = {
    mode = "backtest";
    data.store = "data";
    universe.symbols = ["BTC-USD" "ETH-USD"];
    backtest = {
      start = "2026-01-01T00:00:00Z";
      end = "2026-01-02T00:00:00Z";
      granularity_secs = 86400;
      initial_cash = 10000.0;
      warmup_days = 0;
    };
    strategy.name = "buy-and-hold";
    risk = {
      max_position_frac = 1.0;
      max_total_exposure_frac = 1.0;
      max_daily_loss_frac = 0.5;
      max_order_notional_usd = 1000000.0;
      max_price_deviation_frac = 0.5;
    };
    fees = {
      schedule = "coinbase-advanced-2026-07";
      scale = 1.0;
    };
    execution.slippage_bps = 5.0;
    output.runs_dir = "runs";
    news = {
      store = "news";
      enabled = true;
      llm = {
        backend = "claude";
        model = "claude-opus-5";
        api_key_env = "ANTHROPIC_API_KEY";
      };
    };
  };

  configFile =
    settingsFormat.generate "trading-bot-config.toml"
    (recursiveUpdate defaultSettings cfg.settings);

  # The paper session's config, same relative-path scheme: bind-mounted at
  # /data/paper.toml, so its stores land as siblings under baseDir
  # (data-paper/, paper-state.json, paper-sessions/). Defaults mirror
  # configs/paper-session-atlas.toml in the trading-bot repo; [data],
  # [backtest], and [output] are required by the config schema but ignored
  # by the paper command.
  paperDefaultSettings = {
    mode = "paper";
    data.store = "data-daily";
    universe.symbols = ["BTC-USD" "ETH-USD"];
    backtest = {
      start = "2026-01-01T00:00:00Z";
      end = "2026-08-01T00:00:00Z";
      granularity_secs = 86400;
      initial_cash = 10000.0;
    };
    paper = {
      store = "data-paper";
      initial_cash = 10000.0;
      state_path = "paper-state.json";
      sessions_dir = "paper-sessions";
      # Covers momentum's 90-day lookback with margin, mirroring the
      # baseline's backtest warmup.
      warmup_days = 120;
      status_secs = 300;
      checkpoint_secs = 60;
    };
    strategy = {
      name = "momentum";
      params.lookback_days = 90;
    };
    risk = {
      max_position_frac = 0.6;
      max_total_exposure_frac = 0.95;
      # Unlike the decade-long backtest baselines (which pin this to 1.0 so
      # a historical crash day can't halt the whole run), a live fund wants
      # the kill switch armed: a 15% intraday loss flattens and halts until
      # --reset-kill-switch.
      max_daily_loss_frac = 0.15;
      max_order_notional_usd = 1000000.0;
      max_price_deviation_frac = 0.5;
    };
    fees.schedule = "coinbase-advanced-2026-07";
    execution.slippage_bps = 5.0;
    output.runs_dir = "runs";
  };

  paperConfigFile =
    settingsFormat.generate "trading-bot-paper.toml"
    (recursiveUpdate paperDefaultSettings cfg.paper.settings);

  imageRef = imageLib.renderImage cfg.image;
  registryHost = head (splitString "/" cfg.image.repository);

  newsRun = pkgs.writeShellApplication {
    name = "trading-bot-news-run";
    runtimeInputs = [pkgs.podman];
    text = ''
      # Re-pull every run: the podman auto-prune timer runs with --all,
      # which evicts any image that has no running container, and this one
      # only runs for seconds a day. The registry is the forgejo instance
      # on this same host, whose published web port speaks plain HTTP —
      # hence the pull-scoped --tls-verify=false rather than a host-wide
      # insecure-registry entry.
      podman pull --tls-verify=false ${imageRef}

      run() {
        podman run --rm --name trading-bot-news --pull=never \
          --env-file ${config.sops.templates."trading-bot-news-env".path} \
          --volume ${cfg.baseDir}:/data \
          --volume ${configFile}:/data/config.toml:ro \
          ${imageRef} news --config /data/config.toml "$1"
      }

      run fetch
      run extract
    '';
  };
in {
  options.modules.linux.oci.services.trading-bot = {
    enable = mkEnableOption "trading-bot containers (news fetch/extract cadence, optional paper session)";

    # The self-hosted forgejo registry, reached via its host-published web
    # port so the pull depends on nothing off-host (no pallas Caddy, no
    # pi-hole record, no Pangolin hairpin). The digest-refresh tooling only
    # speaks anonymous TLS registries, so oci-images.json does not manage
    # this image; pushes land on a tag and the image's own OCI revision
    # label carries provenance. Pin image.digest by hand if that ever
    # matters more than convenience.
    image = imageLib.mkImageOptions {
      repository = "localhost:2835/private/trading-bot";
      version = "latest";
    };

    baseDir = mkOption {
      description = "State directory (news store, paper fund state), mounted at /data.";
      type = types.str;
      example = "/data/apps/trading-bot";
    };

    settings = mkOption {
      description = ''
        Recursively merged over the generated trading-bot config.toml. The
        defaults run the news pipeline on BTC-USD/ETH-USD via claude-opus-5.
      '';
      inherit (settingsFormat) type;
      default = {};
    };

    timer = {
      onCalendar = mkOption {
        description = ''
          Fetch/extract cadence. The RSS feeds' horizons are only about six
          days, so anything slower than every few days loses items
          permanently; daily is comfortable.
        '';
        type = types.str;
        default = "05:00";
      };

      randomizedDelaySec = mkOption {
        description = "Jitter applied to each timer firing.";
        type = types.str;
        default = "15m";
      };
    };

    paper = {
      enable = mkEnableOption "long-running paper trading session";

      settings = mkOption {
        description = ''
          Recursively merged over the generated paper.toml. The defaults run
          the momentum strategy on BTC-USD/ETH-USD with the kill switch
          armed at a 15% daily loss.
        '';
        inherit (settingsFormat) type;
        default = {};
      };

      durationMins = mkOption {
        description = "Bound the session length (smoke tests). Null runs until stopped.";
        type = types.nullOr types.ints.positive;
        default = null;
      };

      resetKillSwitch = mkOption {
        description = ''
          Re-arm a tripped kill switch on the next start. Inspect the state
          file and session log first, and revert once the re-armed session
          is running: while set, every container start re-arms the fund.
        '';
        type = types.bool;
        default = false;
      };

      restart = mkOption {
        description = ''
          Restart policy for the session container. The first long session
          runs with "no": the pre-registered checklist in findings.org
          treats a crash as a finding to diagnose before restarting, not a
          condition to paper over. Promote to "on-failure" after a session
          has cleared the checklist.
        '';
        type = types.enum ["no" "on-failure" "always"];
        default = "no";
      };

      networks = mkOption {
        description = "Networks to join. The session only needs egress to Coinbase (HTTPS/WSS).";
        type = types.listOf types.str;
        default = ["default"];
      };
    };

    configProperties = mkOption {
      description = "ZFS properties applied to baseDir.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      assertions = [
        {
          assertion =
            config.modules.linux.oci.registryAuth.enable
            && hasAttr registryHost config.modules.linux.oci.registryAuth.registries;
          message = "trading-bot pulls from the private registry ${registryHost}; give it a modules.linux.oci.registryAuth.registries entry.";
        }
      ];

      modules.linux.oci._managedPaths.${cfg.baseDir}.properties = cfg.configProperties;

      sops.secrets."trading-bot/anthropic-api-key" = {};

      sops.templates."trading-bot-news-env".content = ''
        ANTHROPIC_API_KEY=${config.sops.placeholder."trading-bot/anthropic-api-key"}
      '';

      systemd.services.trading-bot-news = {
        description = "trading-bot news fetch/extract";
        # Not an oci-containers unit, so registry-auth.nix's blanket env
        # injection misses it — wire the authfile in directly.
        environment.REGISTRY_AUTH_FILE = config.modules.linux.oci.registryAuth.authFile;
        after =
          ["network-online.target"]
          ++ optional config.modules.linux.oci.zfs.enable "zfs-manage-datasets.service"
          # When the registry-hosting forgejo lives on this same host, don't
          # race its container at boot; elsewhere the pull just retries on
          # the next firing.
          ++ optional config.modules.linux.oci.services.forgejo.enable "podman-forgejo.service";
        requires = optional config.modules.linux.oci.zfs.enable "zfs-manage-datasets.service";
        wants =
          ["network-online.target"]
          ++ optional config.modules.linux.oci.services.forgejo.enable "podman-forgejo.service";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = getExe newsRun;
          # A run against a large backlog spends a long time in the LLM
          # extraction pass, and Type=oneshot counts the whole run as startup.
          TimeoutStartSec = "3h";
        };
      };

      systemd.timers.trading-bot-news = {
        wantedBy = ["timers.target"];
        timerConfig = {
          OnCalendar = cfg.timer.onCalendar;
          RandomizedDelaySec = cfg.timer.randomizedDelaySec;
          # A firing missed while the host is down still runs at boot — with
          # ~six-day feed horizons, a skipped day can be data lost for good.
          Persistent = true;
        };
      };
    }

    (mkIf cfg.paper.enable {
      warnings =
        optional cfg.paper.resetKillSwitch
        "trading-bot: paper.resetKillSwitch re-arms the kill switch on every container start — revert it once the re-armed session is running.";

      modules.linux.oci.networks = listToAttrs (
        map (n: nameValuePair n {enable = true;}) cfg.paper.networks
      );

      virtualisation.oci-containers.containers.trading-bot-paper = {
        image = imageRef;
        cmd =
          ["paper" "--config" "/data/paper.toml"]
          ++ optionals (cfg.paper.durationMins != null) ["--duration-mins" (toString cfg.paper.durationMins)]
          ++ optionals cfg.paper.resetKillSwitch ["--reset-kill-switch"];
        volumes = [
          "${cfg.baseDir}:/data"
          "${paperConfigFile}:/data/paper.toml:ro"
        ];
        extraOptions =
          ["--network-alias=trading-bot-paper"]
          ++ (map (n: "--network=${ociLib.networkName n}") cfg.paper.networks)
          ++ [
            # Unlike the news job, this container pulls inside podman run,
            # so the pull-scoped plain-HTTP flag rides along here.
            "--tls-verify=false"
            # Graceful shutdown (final checkpoint + fund-lifetime report)
            # triggers on SIGINT only. The image sets StopSignal=SIGINT;
            # pinning it here keeps the wiring correct even against an
            # image built before that label existed.
            "--stop-signal=SIGINT"
          ]
          ++ imageLib.mkImageLabels {
            module = "trading-bot.paper";
            inherit (cfg) image;
          };
        log-driver = "journald";
      };

      systemd.services."podman-trading-bot-paper" = mkMerge [
        (ociLib.mkServiceConfig {
          networks = cfg.paper.networks;
          extraAfter =
            ["network-online.target"]
            ++ optional config.modules.linux.oci.services.forgejo.enable "podman-forgejo.service";
        })
        {
          wants =
            ["network-online.target"]
            ++ optional config.modules.linux.oci.services.forgejo.enable "podman-forgejo.service";
          # The generated preStop runs `podman stop` with no --time, and
          # podman's 10s default would SIGKILL the session mid-shutdown; it
          # needs up to ~90s to write the final checkpoint and report.
          # mkForce because preStop is lines-typed — a plain definition
          # would concatenate with the generated stop instead of replacing
          # it. Keep --time below the unit's TimeoutStopSec (120) so podman
          # owns the SIGKILL escalation, not systemd.
          preStop = mkForce "podman stop --ignore --time 90 --cidfile=/run/trading-bot-paper/ctr-id";
          # A session crash is a checklist finding to diagnose, not a
          # condition to restart through — override mkServiceConfig's
          # blanket Restart=always with the supervision policy.
          serviceConfig.Restart = mkForce cfg.paper.restart;
        }
      ];
    })
  ]);
}
