{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.tumblr-alt-text-bot;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.tumblr-alt-text-bot = {
    enable = mkEnableOption "Tumblr alt-text bot";

    image = imageLib.mkImageOptions {
      repository = "codeberg.org/ryf/tumblr-alt-text-bot";
      version = "latest";
    };

    baseDir = mkOption {
      description = ''
        State directory for the bot, mounted at /data inside the container.
        Holds the last-handled-notification checkpoint, the handled-post
        history, and the rotating OAuth token cache — all created 0600 by the
        app. Persist it so the bot doesn't re-describe posts or re-auth after a
        restart.
      '';
      type = types.str;
      example = "/zroot/apps/tumblr-alt-text-bot";
    };

    botBlog = mkOption {
      description = "Blog the bot posts from and watches for mentions (no leading @).";
      type = types.str;
      example = "alt-text-bot";
    };

    visionModel = mkOption {
      description = ''
        Model id for the vision backend. Null defers to the binary's own
        default (currently a Claude Haiku snapshot), so the image can change
        the default without a config edit here.
      '';
      type = types.nullOr types.str;
      default = null;
    };

    pollIntervalSecs = mkOption {
      description = "Seconds between notification polls. Null uses the binary default (60).";
      type = types.nullOr types.ints.positive;
      default = null;
    };

    networks = mkOption {
      description = "Networks to join. The bot only needs egress, so the shared default bridge suffices.";
      type = types.listOf types.str;
      default = ["default"];
    };

    dependsOn = mkOption {
      description = "Other oci-containers this service depends on.";
      type = types.listOf types.str;
      default = [];
    };

    extraEnv = mkOption {
      description = "Extra environment variables (e.g. VISION_BACKEND, HISTORY_CAPACITY, RUST_LOG).";
      type = types.attrsOf types.str;
      default = {};
    };

    configProperties = mkOption {
      description = "ZFS properties applied to the baseDir dataset. State is a handful of small JSON/text files.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci._managedPaths.${cfg.baseDir}.properties = cfg.configProperties;

    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    # Only stable, non-rotating credentials live in sops. The OAuth access and
    # refresh tokens rotate over the bot's lifetime and are owned by the on-disk
    # token cache (token.json), so they are deliberately kept out of declarative
    # config — see the TUMBLR_ACCESS_TOKEN note below and the baseDir seeding step.
    sops.secrets = {
      "tumblr-alt-text-bot/consumer-key" = {};
      "tumblr-alt-text-bot/consumer-secret" = {};
      "tumblr-alt-text-bot/vision-api-key" = {};
    };

    sops.templates."tumblr-alt-text-bot-env".content = ''
      TUMBLR_CONSUMER_KEY=${config.sops.placeholder."tumblr-alt-text-bot/consumer-key"}
      TUMBLR_CONSUMER_SECRET=${config.sops.placeholder."tumblr-alt-text-bot/consumer-secret"}
      VISION_API_KEY=${config.sops.placeholder."tumblr-alt-text-bot/vision-api-key"}
    '';

    virtualisation.oci-containers.containers.tumblr-alt-text-bot = {
      image = imageLib.renderImage cfg.image;
      inherit (cfg) dependsOn;
      environment =
        {
          BOT_BLOG = cfg.botBlog;
          # The binary requires TUMBLR_ACCESS_TOKEN to be set, but the on-disk
          # token cache (token.json, seeded once from `auth`) is authoritative
          # whenever present, so this value is never used at steady state. It is
          # an inert sentinel — not a sops secret — precisely so the rotating
          # OAuth tokens stay out of declarative config. If the cache is missing
          # the bot seeds from this stub and fails loudly on the first API call,
          # which is the intended signal to re-seed token.json via `auth`.
          TUMBLR_ACCESS_TOKEN = "token-json-is-authoritative";
        }
        // optionalAttrs (cfg.visionModel != null) {VISION_MODEL = cfg.visionModel;}
        // optionalAttrs (cfg.pollIntervalSecs != null) {POLL_INTERVAL_SECS = toString cfg.pollIntervalSecs;}
        // cfg.extraEnv;
      environmentFiles = [config.sops.templates."tumblr-alt-text-bot-env".path];
      # `:U` chowns the dataset to the image's baked-in 65534:65534 user; under
      # rootful podman that uid maps straight to the host, and the ZFS dataset
      # is otherwise root-owned and unwritable by the container.
      volumes = ["${cfg.baseDir}:/data:U"];
      extraOptions =
        ["--network-alias=tumblr-alt-text-bot"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ imageLib.mkImageLabels {
          module = "tumblr-alt-text-bot";
          image = cfg.image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-tumblr-alt-text-bot" = ociLib.mkServiceConfig {
      networks = cfg.networks;
      sopsTemplates = ["tumblr-alt-text-bot-env"];
    };
  };
}
