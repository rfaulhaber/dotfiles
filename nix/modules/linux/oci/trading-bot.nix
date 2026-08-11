{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.trading-bot;
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
        # The research feed is valid only at a single model/prompt version
        # (trading-bot design §4.2); change deliberately, never per-host.
        model = "claude-opus-5";
        api_key_env = "ANTHROPIC_API_KEY";
      };
    };
  };

  configFile =
    settingsFormat.generate "trading-bot-config.toml"
    (recursiveUpdate defaultSettings cfg.settings);

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
    enable = mkEnableOption "trading-bot news fetch/extract cadence";

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
      description = "State directory (news store), mounted at /data.";
      type = types.str;
      example = "/data/apps/trading-bot";
    };

    settings = mkOption {
      description = ''
        Recursively merged over the generated trading-bot config.toml. The
        defaults run the news pipeline on BTC-USD/ETH-USD via claude-opus-5.
      '';
      type = settingsFormat.type;
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

    configProperties = mkOption {
      description = "ZFS properties applied to baseDir.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };
  };

  config = mkIf cfg.enable {
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
  };
}
