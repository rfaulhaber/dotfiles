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

  # slskd.yml is built as a JSON-as-YAML payload and rendered through a
  # sops template so the api_keys (sops placeholders) get substituted at
  # activation time.
  configYamlAttrs = {
    shares.directories = cfg.shares;
    web.authentication.api_keys =
      mapAttrs (_name: k: {
        key = config.sops.placeholder.${k.secretName};
        inherit (k) role;
      })
      cfg.apiKeys;
    inherit (cfg) retention;
  };
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
      description = "Host directory for completed Soulseek downloads (mounted at /app/downloads).";
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

    grpcPort = mkOption {
      description = "Host port for slskd's HTTPS/gRPC API.";
      type = types.port;
      default = 5031;
    };

    peerPort = mkOption {
      description = "Host port for inbound Soulseek peer connections.";
      type = types.port;
      default = 50300;
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
        Whether to allow editing slskd.yml from the web UI. Now that Nix
        manages slskd.yml, leaving this on lets you tweak settings from
        the UI in-session (overwritten on next rebuild). Turn it off to
        force fully-declarative config.
      '';
      type = types.bool;
      default = true;
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
      description = "ZFS properties applied to the baseDir dataset. Defaults tuned for LiteDB.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };

    # ----- slskd.yml fields --------------------------------------------

    shares = mkOption {
      description = "Directories shared with the Soulseek network. Paths are container-side.";
      type = types.listOf types.str;
      default = ["/music"];
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

    retention = mkOption {
      description = ''
        slskd retention policies (minutes for time-based, except `logs`
        which is days). Mirrors the slskd.yml layout 1:1 — see slskd
        docs for the field semantics.
      '';
      type = types.attrs;
      default = {
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
          };
        };
        files = {
          complete = 20160;
          incomplete = 43200;
        };
        logs = 180;
      };
    };
  };

  config = mkIf cfg.enable (let
    portMappings = [
      "${toString cfg.webPort}:5030"
      "${toString cfg.grpcPort}:5031"
      "${toString cfg.peerPort}:50300"
    ];
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

    # Land slskd.yml owned by the container UID so slskd can rewrite it
    # at runtime (UI edits, scan state). Nix wins on the next rebuild.
    configInitScript = pkgs.writeShellScript "slskd-config-init" ''
      install -m 0640 -o ${toString cfg.user.uid} -g ${toString cfg.user.gid} \
        ${config.sops.templates."slskd-yml".path} \
        ${cfg.baseDir}/slskd.yml
    '';
  in {
    sops.secrets =
      {
        "slskd/username" = {};
        "slskd/password" = {};
      }
      // listToAttrs (mapAttrsToList (_: k: nameValuePair k.secretName {}) cfg.apiKeys);

    sops.templates = {
      "slskd-env".content = ''
        SLSKD_SLSK_USERNAME=${config.sops.placeholder."slskd/username"}
        SLSKD_SLSK_PASSWORD=${config.sops.placeholder."slskd/password"}
      '';
      "slskd-yml" = {
        content = builtins.toJSON configYamlAttrs;
        # Read-only by other UIDs; the ExecStartPre cp lands a copy into
        # baseDir owned by the container UID, so the container reads its
        # own writable copy and not this template directly.
        mode = "0400";
      };
    };

    virtualisation.oci-containers.containers.slskd = {
      image = imageLib.renderImage cfg.image;
      inherit (cfg) dependsOn;
      environment = {
        "SLSKD_REMOTE_CONFIGURATION" =
          if cfg.remoteConfiguration
          then "true"
          else "false";
        "TZ" = cfg.timezone;
      };
      environmentFiles = [config.sops.templates."slskd-env".path];
      volumes = [
        "${cfg.baseDir}:/app"
        "${cfg.downloadsDir}:/app/downloads"
        "${cfg.musicDir}:/music:ro"
      ];
      ports = optionals (!cfg.useGluetun) portMappings;
      extraOptions =
        netOpts
        ++ ["--user=${toString cfg.user.uid}:${toString cfg.user.gid}"];
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

      networks = mkIf (!cfg.useGluetun) (
        listToAttrs (map (n: nameValuePair n {enable = true;}) cfg.networks)
      );
    };
  });
}
