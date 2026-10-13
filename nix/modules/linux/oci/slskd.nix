{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.slskd;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
  yamlFormat = pkgs.formats.yaml {};

  # API consumers (e.g. soularr) authenticate against slskd via a named
  # api_key entry. The key value is a sops secret; the consumer name is
  # an arbitrary label.
  apiKeyOpts = _: {
    options = {
      secretName = mkOption {
        description = ''
          Sops secret name holding the API key for this consumer. Module
          declares `sops.secrets.<secretName>` automatically.
        '';
        type = types.str;
        example = "soularr/slskd-api-key";
      };
      role = mkOption {
        description = "Role granted to this API key.";
        type = types.enum ["readonly" "readwrite" "administrator"];
        default = "readwrite";
      };
    };
  };

  # mkDefault on every leaf, so a host overriding one key keeps its
  # siblings. mapAttrsRecursive treats lists as leaves, so a host's
  # shares.directories replaces the default rather than appending.
  defaultSettings = mapAttrsRecursive (_: mkDefault) {
    shares = {
      directories = ["/music"];
      # Minutes. Without it slskd never rescans, and a restart only rescans
      # when share options change, so new imports would stay unshared.
      cache.retention = 1440;
    };
    retention = {
      search = 10080;
      transfers = {
        upload = {
          succeeded = 1440;
          errored = 30;
          cancelled = 5;
        };
        download = {
          succeeded = 1440;
          errored = 20160;
          cancelled = 5;
          # `errored` doesn't cover rejected downloads; `failed` covers
          # every unsuccessful state.
          failed = 20160;
        };
      };
      files = {
        complete = 20160;
        incomplete = 43200;
      };
    };
    transfers = {
      # Per-user limits, enforced when a peer enqueues. Over-limit requests
      # get the standard "Too many files/megabytes" reply, which clients
      # treat as retry-later.
      upload.limits = {
        queued = {
          files = 500;
          megabytes = 5120;
        };
        weekly.megabytes = 51200;
      };
      groups.leechers = {
        thresholds = {
          files = 50;
          directories = 5;
        };
        upload = {
          priority = 999;
          slots = 2;
          speed_limit = 256;
        };
      };
    };
    # stdout already lands in journald.
    logger.no_disk = true;
    # 5031 would serve a self-signed cert regenerated on every start;
    # TLS terminates at the reverse proxy.
    web.https.disabled = true;
    flags.no_version_check = true;
  };

  # Keys tied to this module's mounts, port mapping and secrets. A host
  # setting a different value gets an evaluation conflict.
  ownedSettings = {
    remote_configuration = cfg.remoteConfiguration;
    # Incomplete files share the downloads bind mount so completion is a
    # rename, not a cross-dataset copy. The dot keeps it out of slskd's
    # complete-file pruning, which skips hidden entries.
    directories = {
      downloads = "/app/downloads";
      incomplete = "/app/downloads/.incomplete";
    };
    web = {
      port = 5030;
      authentication.api_keys =
        mapAttrs (_name: k: {
          key = config.sops.placeholder.${k.secretName};
          inherit (k) role;
        })
        cfg.apiKeys;
    };
  };

  # Secrets travel through the env file, which podman reads literally.
  # slskd.yml is JSON with secret text substituted in raw, so a quote or
  # backslash in a secret would corrupt it. slskd lets slskd.yml override
  # env vars, so the same key in settings would silently replace the secret.
  envSecretPaths =
    [
      ["web" "authentication" "password"]
      ["web" "authentication" "jwt" "key"]
      ["soulseek" "username"]
      ["soulseek" "password"]
    ]
    ++ optional (cfg.listenPortSecret != null) ["soulseek" "listen_port"];
  # slskd skips null YAML values, so an explicit null shadows nothing.
  shadowedSecretPaths = filter (p: attrByPath p null cfg.settings != null) envSecretPaths;
in {
  options.modules.linux.oci.services.slskd = {
    enable = mkEnableOption "slskd Soulseek client";

    image = imageLib.mkImageOptions {
      repository = "slskd/slskd";
      version = "latest";
    };

    baseDir = mkOption {
      description = ''
        Base directory for slskd state. Mounted at /app inside the container
        (slskd's config layout uses /app, not /config). slskd.yml itself is
        copied in from the sops template at boot via ExecStartPre — slskd
        rewrites the file when settings change in the UI, so a read-only
        bind mount would cause errors.
      '';
      type = types.str;
      example = "/data/apps/slskd";
    };

    downloadsDir = mkOption {
      description = ''
        Host directory for Soulseek downloads (mounted at /app/downloads).
        In-progress files live in its .incomplete subdirectory.
      '';
      type = types.str;
      example = "/data/slskd";
    };

    musicDir = mkOption {
      description = "Host directory for the music library to share with Soulseek (mounted read-only at /music).";
      type = types.str;
      example = "/data/music";
    };

    webPort = mkOption {
      description = "Host port for the slskd web UI.";
      type = types.port;
      default = 5030;
    };

    user = {
      uid = mkOption {
        description = "UID to run the container as (slskd uses --user, not PUID).";
        type = types.int;
        default = 1000;
      };
      gid = mkOption {
        description = "GID to run the container as.";
        type = types.int;
        default = 100;
      };
    };

    remoteConfiguration = mkOption {
      description = ''
        Whether to allow editing slskd.yml from the web UI. Anyone who can
        log in could then add integrations.scripts entries, which slskd runs
        as shell commands. Edits don't persist either: slskd.yml is copied
        from Nix on every service start.
      '';
      type = types.bool;
      default = false;
    };

    webPasswordSecret = mkOption {
      description = "Sops secret holding the web UI password. Declared by this module.";
      type = types.str;
      default = "slskd/web-password";
    };

    jwtKeySecret = mkOption {
      description = ''
        Sops secret holding the JWT signing key (32 to 255 characters).
        Without a stable key slskd generates one per process, and every
        restart logs the UI out. Declared by this module.
      '';
      type = types.str;
      default = "slskd/jwt-key";
    };

    listenPortSecret = mkOption {
      description = ''
        Sops secret holding the Soulseek listen port (1024 to 65535). It
        must equal the port the VPN provider forwards, with local port =
        remote port: slskd advertises its listen port to the Soulseek server
        as-is. Also opened on gluetun's VPN interface, so this requires
        useGluetun. Null keeps slskd's default port, unreachable to inbound
        peers.
      '';
      type = types.nullOr types.str;
      default = null;
      example = "slskd/listen-port";
    };

    timezone = mkOption {
      description = "Timezone for the container.";
      type = types.str;
      default = "America/New_York";
    };

    useGluetun = mkOption {
      description = ''
        Route all traffic through the gluetun VPN container by joining its
        network namespace. Required for slskd: the Soulseek protocol
        identifies clients by username + IP, both of which need to come
        from the VPN endpoint, not the host.
      '';
      type = types.bool;
      default = false;
    };

    gluetunContainer = mkOption {
      description = "Name of the gluetun container to share netns with.";
      type = types.str;
      default = "gluetun";
    };

    networks = mkOption {
      description = "Networks to join (only used when useGluetun = false).";
      type = types.listOf types.str;
      default = ["default"];
    };

    dependsOn = mkOption {
      description = "Other oci-containers this service depends on.";
      type = types.listOf types.str;
      default = [];
    };

    configProperties = mkOption {
      description = "ZFS properties applied to the baseDir dataset, which holds slskd's SQLite databases.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };

    settings = mkOption {
      description = ''
        slskd.yml contents, mirroring slskd's YAML layout. The module
        supplies overridable defaults and owns the keys tied to its mounts,
        port mapping and API keys (remote_configuration, directories.*,
        web.port, web.authentication.api_keys).

        Values land in the Nix store, so secrets don't belong here. The web
        password, JWT key, Soulseek login and listen port come from sops via
        the env file; setting those keys here is an evaluation error.
      '';
      type = types.submodule {freeformType = yamlFormat.type;};
      default = {};
      example = literalExpression ''
        {
          transfers.upload.speed_limit = 10240;
        }
      '';
    };

    apiKeys = mkOption {
      description = ''
        Named API keys for programmatic access. Each entry maps a consumer
        name (e.g. "soularr") to a sops secret holding its API key.
      '';
      type = types.attrsOf (types.submodule apiKeyOpts);
      default = {};
      example = literalExpression ''
        {
          soularr = {
            secretName = "soularr/slskd-api-key";
            role = "readwrite";
          };
        }
      '';
    };
  };

  config = mkIf cfg.enable (let
    portMappings = ["${toString cfg.webPort}:5030"];
    netOpts =
      (
        if cfg.useGluetun
        then ["--network=container:${cfg.gluetunContainer}"]
        else
          ["--network-alias=slskd"]
          ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
      )
      ++ imageLib.mkImageLabels {
        module = "slskd";
        inherit (cfg) image;
      };
    gluetunDeps = optional cfg.useGluetun "podman-${cfg.gluetunContainer}.service";
    uidGid = "-o ${toString cfg.user.uid} -g ${toString cfg.user.gid}";

    # Land slskd.yml owned by the container UID so slskd can rewrite it
    # if remote configuration is enabled. Nix wins on the next start. slskd
    # refuses a custom incomplete directory that doesn't exist yet.
    configInitScript = pkgs.writeShellScript "slskd-config-init" ''
      install -m 0640 ${uidGid} \
        ${config.sops.templates."slskd-yml".path} \
        ${cfg.baseDir}/slskd.yml
      install -d -m 0755 ${uidGid} ${cfg.downloadsDir}/.incomplete
    '';
  in {
    assertions = [
      {
        assertion = shadowedSecretPaths == [];
        message = ''
          modules.linux.oci.services.slskd.settings sets
          ${concatMapStringsSep ", " (concatStringsSep ".") shadowedSecretPaths}.
          The module delivers these from sops through the env file, and
          slskd.yml overrides env, so the settings value would silently
          replace the secret.
        '';
      }
      {
        assertion = cfg.listenPortSecret != null -> cfg.useGluetun;
        message = ''
          modules.linux.oci.services.slskd.listenPortSecret requires
          useGluetun. The port is secret, so it can't become a host port
          mapping; only gluetun's VPN input ports can open it to peers.
        '';
      }
    ];

    modules.linux.oci.services.slskd.settings = mkMerge [defaultSettings ownedSettings];

    sops.secrets =
      {
        "slskd/username" = {};
        "slskd/password" = {};
        ${cfg.webPasswordSecret} = {};
        ${cfg.jwtKeySecret} = {};
      }
      // optionalAttrs (cfg.listenPortSecret != null) {${cfg.listenPortSecret} = {};}
      // listToAttrs (mapAttrsToList (_: k: nameValuePair k.secretName {}) cfg.apiKeys);

    sops.templates = {
      "slskd-env".content =
        ''
          SLSKD_SLSK_USERNAME=${config.sops.placeholder."slskd/username"}
          SLSKD_SLSK_PASSWORD=${config.sops.placeholder."slskd/password"}
          SLSKD_PASSWORD=${config.sops.placeholder.${cfg.webPasswordSecret}}
          SLSKD_JWT_KEY=${config.sops.placeholder.${cfg.jwtKeySecret}}
        ''
        + optionalString (cfg.listenPortSecret != null) ''
          SLSKD_SLSK_LISTEN_PORT=${config.sops.placeholder.${cfg.listenPortSecret}}
        '';
      # JSON is valid YAML. Rendered through sops so the api_keys
      # placeholders get substituted at activation time.
      "slskd-yml" = {
        content = builtins.toJSON cfg.settings;
        # Read-only by other UIDs; the ExecStartPre cp lands a copy into
        # baseDir owned by the container UID, so the container reads its
        # own writable copy and not this template directly.
        mode = "0400";
      };
    };

    virtualisation.oci-containers.containers.slskd = {
      image = imageLib.renderImage cfg.image;
      inherit (cfg) dependsOn;
      environment."TZ" = cfg.timezone;
      environmentFiles = [config.sops.templates."slskd-env".path];
      volumes = [
        "${cfg.baseDir}:/app"
        "${cfg.downloadsDir}:/app/downloads"
        "${cfg.musicDir}:/music:ro"
      ];
      ports = optionals (!cfg.useGluetun) portMappings;
      extraOptions =
        netOpts
        ++ [
          "--user=${toString cfg.user.uid}:${toString cfg.user.gid}"
          # The peer port is internet-facing. slskd runs unprivileged and its
          # --user entrypoint needs no capabilities, so a compromise gets none.
          "--cap-drop=all"
          "--security-opt=no-new-privileges:true"
        ];
      log-driver = "journald";
    };

    systemd.services."podman-slskd" = mkMerge [
      (ociLib.mkServiceConfig {
        networks =
          if cfg.useGluetun
          then []
          else cfg.networks;
        extraAfter = gluetunDeps;
        extraRequires = gluetunDeps;
        sopsTemplates = ["slskd-env" "slskd-yml"];
      })
      {
        serviceConfig.ExecStartPre = ["${configInitScript}"];
      }
    ];

    modules.linux.oci = {
      _managedPaths.${cfg.baseDir}.properties = cfg.configProperties;
      _gluetunPorts = mkIf cfg.useGluetun portMappings;
      _gluetunVpnInputPortSecrets = optional (cfg.useGluetun && cfg.listenPortSecret != null) cfg.listenPortSecret;

      networks = mkIf (!cfg.useGluetun) (
        listToAttrs (map (n: nameValuePair n {enable = true;}) cfg.networks)
      );
    };
  });
}
