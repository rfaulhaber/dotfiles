{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.requestrr;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};

  # *arr download client option block — shared between Radarr/Sonarr/Lidarr.
  arrClientOpts = {
    options = {
      enable = mkEnableOption "this download client";

      hostname = mkOption {
        description = "Hostname requestrr connects to.";
        type = types.str;
        default = "";
      };

      port = mkOption {
        description = "Port requestrr connects to.";
        type = types.port;
      };

      apiKeySecret = mkOption {
        description = ''
          Sops secret name holding the *arr API key. Reuse the same name
          across modules (e.g., recyclarr/sonarr-main-api-key) to keep
          one source of truth per *arr instance.
        '';
        type = types.nullOr types.str;
        default = null;
      };

      baseUrl = mkOption {
        description = "URL base prefix on the *arr instance.";
        type = types.str;
        default = "";
      };

      useSSL = mkOption {
        description = "Connect via HTTPS.";
        type = types.bool;
        default = false;
      };

      version = mkOption {
        description = "*arr API version string.";
        type = types.str;
      };

      searchNewRequests = mkOption {
        type = types.bool;
        default = true;
      };

      monitorNewRequests = mkOption {
        type = types.bool;
        default = true;
      };

      categories = mkOption {
        description = ''
          Per-instance categories. Free-form attrset list — see requestrr's
          UI for which fields each kind expects (Radarr/Sonarr/Lidarr have
          different shapes). Rendered into Settings.json verbatim.
        '';
        type = types.listOf types.attrs;
        default = [];
      };
    };
  };

  # Render an arr client subobject. apiKeySecret -> sops placeholder, or
  # empty string when the client is disabled / not configured.
  renderArrClient = client: extraDefaults:
    {
      Hostname = client.hostname;
      Port = client.port;
      ApiKey =
        if client.apiKeySecret != null
        then config.sops.placeholder.${client.apiKeySecret}
        else "";
      BaseUrl = client.baseUrl;
      UseSSL = client.useSSL;
      Version = client.version;
      SearchNewRequests = client.searchNewRequests;
      MonitorNewRequests = client.monitorNewRequests;
      Categories = client.categories;
    }
    // extraDefaults;

  # Map a list of sops secret names → list of placeholder strings. The
  # placeholders get substituted at template render time, yielding a JSON
  # array of literal channel IDs.
  channelPlaceholders = secretNames:
    map (n: config.sops.placeholder.${n}) secretNames;

  settingsJsonAttrs = {
    Authentication = {
      Username = config.sops.placeholder.${cfg.auth.usernameSecret};
      Password = config.sops.placeholder."requestrr/auth-password-hash";
      PrivateKey = config.sops.placeholder."requestrr/auth-private-key";
    };

    ChatClients = {
      Discord = {
        BotToken = config.sops.placeholder."requestrr/discord-bot-token";
        ClientId = config.sops.placeholder.${cfg.discord.clientIdSecret};
        StatusMessage = cfg.discord.statusMessage;
        TvShowRoles = cfg.discord.tvShowRoles;
        MovieRoles = cfg.discord.movieRoles;
        MusicRoles = cfg.discord.musicRoles;
        MonitoredChannels = channelPlaceholders cfg.discord.monitoredChannelSecrets;
        EnableRequestsThroughDirectMessages = cfg.discord.enableRequestsThroughDirectMessages;
        AutomaticallyNotifyRequesters = cfg.discord.automaticallyNotifyRequesters;
        NotificationMode = cfg.discord.notificationMode;
        NotificationChannels = channelPlaceholders cfg.discord.notificationChannelSecrets;
        AutomaticallyPurgeCommandMessages = cfg.discord.automaticallyPurgeCommandMessages;
      };
      Language = cfg.language;
    };

    DownloadClients = {
      Ombi = {
        Hostname = "";
        Port = 3579;
        ApiKey = "";
        ApiUsername = "";
        BaseUrl = "";
        UseSSL = false;
        Version = "3";
        UseMovieIssue = false;
        UseTVIssue = false;
      };
      Overseerr = {
        Hostname = "";
        Port = 5055;
        ApiKey = "";
        BaseUrl = "";
        Movies.DefaultApiUserID = "";
        Movies.Categories = [];
        TvShows.DefaultApiUserID = "";
        TvShows.Categories = [];
        UseSSL = false;
        Version = "1";
        UseMovieIssue = false;
        UseTVIssue = false;
      };
      Radarr = renderArrClient cfg.radarr {};
      Sonarr = renderArrClient cfg.sonarr {};
      Lidarr = renderArrClient cfg.lidarr {};
    };

    BotClient.Client = "Discord";
    Movies.Client =
      if cfg.radarr.enable
      then "Radarr"
      else "Disabled";
    TvShows = {
      Client =
        if cfg.sonarr.enable
        then "Sonarr"
        else "Disabled";
      Restrictions = cfg.sonarr.restrictions;
    };
    Music.Client =
      if cfg.lidarr.enable
      then "Lidarr"
      else "Disabled";
    Port = cfg.webPort;
    BaseUrl = cfg.baseUrl;
    DisableAuthentication = cfg.disableAuthentication;
    Version = cfg.version;
  };
in {
  options.modules.linux.oci.services.requestrr = {
    enable = mkEnableOption "Requestrr chat-bot media-request frontend";

    image = imageLib.mkImageOptions {
      repository = "thomst08/requestrr";
      version = "latest";
    };

    baseDir = mkOption {
      description = ''
        Base directory for requestrr config (mounted at /root/config inside
        the container — requestrr runs as root and stores Settings.json
        there). Settings.json is copied in from the sops template at boot;
        requestrr rewrites it on shutdown, so a read-only bind would error.
      '';
      type = types.str;
      example = "/data/apps/requestrr";
    };

    webPort = mkOption {
      description = "Host port for the requestrr web UI (forwarded onto gluetun when useGluetun = true).";
      type = types.port;
      default = 4545;
    };

    timezone = mkOption {
      description = "Timezone for the container.";
      type = types.str;
      default = "America/New_York";
    };

    useGluetun = mkOption {
      description = ''
        Route through the gluetun VPN container's network namespace.
        Recommended: requestrr maintains a long-lived websocket connection
        to Discord (or other chat platforms), and the source IP of that
        connection is identifying.
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
      description = "ZFS properties applied to the baseDir dataset.";
      type = types.attrsOf types.str;
      default = {};
    };

    # ----- Settings.json fields -----------------------------------------

    version = mkOption {
      description = "Requestrr config schema version (matches the requestrr release).";
      type = types.str;
      default = "2.1.3";
    };

    baseUrl = mkOption {
      description = "BaseUrl prefix when behind a path-based reverse proxy.";
      type = types.str;
      default = "";
    };

    disableAuthentication = mkOption {
      description = "If true, requestrr's web UI doesn't ask for credentials.";
      type = types.bool;
      default = false;
    };

    language = mkOption {
      description = "Bot/UI language code.";
      type = types.str;
      default = "english";
    };

    auth.usernameSecret = mkOption {
      description = ''
        Sops secret name holding the web UI username. The matching
        password hash and PrivateKey live at requestrr/auth-password-hash
        and requestrr/auth-private-key — generate by running requestrr
        once and copying values from the resulting Settings.json.
      '';
      type = types.str;
      default = "requestrr/auth-username";
    };

    discord = {
      clientIdSecret = mkOption {
        description = ''
          Sops secret name holding the Discord application client ID.
          Treated as identifying material per project policy — kept out
          of the world-readable nix store.
        '';
        type = types.str;
        default = "requestrr/discord-client-id";
      };

      statusMessage = mkOption {
        description = "Bot status text.";
        type = types.str;
        default = "/help";
      };

      monitoredChannelSecrets = mkOption {
        description = ''
          Sops secret names — one per Discord channel ID the bot listens
          on. Each named secret holds a single channel snowflake; the
          rendered Settings.json contains a JSON array of the resolved
          values.
        '';
        type = types.listOf types.str;
        default = [];
      };

      notificationChannelSecrets = mkOption {
        description = "Sops secret names, one per channel ID the bot posts notifications to.";
        type = types.listOf types.str;
        default = [];
      };

      notificationMode = mkOption {
        type = types.enum ["Channels" "DirectMessages" "Both"];
        default = "Channels";
      };

      tvShowRoles = mkOption {
        type = types.listOf types.str;
        default = [];
      };

      movieRoles = mkOption {
        type = types.listOf types.str;
        default = [];
      };

      musicRoles = mkOption {
        type = types.listOf types.str;
        default = [];
      };

      enableRequestsThroughDirectMessages = mkOption {
        type = types.bool;
        default = false;
      };

      automaticallyNotifyRequesters = mkOption {
        type = types.bool;
        default = true;
      };

      automaticallyPurgeCommandMessages = mkOption {
        type = types.bool;
        default = true;
      };
    };

    radarr = mkOption {
      description = "Radarr download client config.";
      type = types.submodule {
        options =
          arrClientOpts.options
          // {
            port = mkOption {
              type = types.port;
              default = 7878;
            };
            version = mkOption {
              type = types.str;
              default = "3";
            };
          };
      };
      default = {};
    };

    sonarr = mkOption {
      description = "Sonarr download client config.";
      type = types.submodule {
        options =
          arrClientOpts.options
          // {
            port = mkOption {
              type = types.port;
              default = 8989;
            };
            version = mkOption {
              type = types.str;
              default = "4";
            };
            restrictions = mkOption {
              description = ''
                Sonarr-only TV restrictions field (None, AnimeOnly, etc.)
              '';
              type = types.str;
              default = "None";
            };
          };
      };
      default = {};
    };

    lidarr = mkOption {
      description = "Lidarr download client config.";
      type = types.submodule {
        options =
          arrClientOpts.options
          // {
            port = mkOption {
              type = types.port;
              default = 8686;
            };
            version = mkOption {
              type = types.str;
              default = "1";
            };
          };
      };
      default = {};
    };
  };

  config = mkIf cfg.enable (let
    portMappings = ["${toString cfg.webPort}:4545"];
    netOpts =
      (
        if cfg.useGluetun
        then ["--network=container:${cfg.gluetunContainer}"]
        else
          ["--network-alias=requestrr"]
          ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
      )
      ++ imageLib.mkImageLabels {
        module = "requestrr";
        image = cfg.image;
      };
    gluetunDeps = optional cfg.useGluetun "podman-${cfg.gluetunContainer}.service";

    # Collect every sops secret the rendered Settings.json references.
    referencedApiKeySecrets =
      filter (s: s != null)
      (map (c: c.apiKeySecret) [cfg.radarr cfg.sonarr cfg.lidarr]);

    # Identifying-info secrets (auth username, Discord IDs).
    identifyingSecretNames =
      [
        cfg.auth.usernameSecret
        cfg.discord.clientIdSecret
      ]
      ++ cfg.discord.monitoredChannelSecrets
      ++ cfg.discord.notificationChannelSecrets;

    configInitScript = pkgs.writeShellScript "requestrr-config-init" ''
      mkdir -p ${cfg.baseDir}
      install -m 0600 ${config.sops.templates."requestrr-settings-json".path} \
        ${cfg.baseDir}/settings.json
    '';
  in {
    sops.secrets =
      {
        "requestrr/auth-password-hash" = {};
        "requestrr/auth-private-key" = {};
        "requestrr/discord-bot-token" = {};
      }
      // listToAttrs (map (n: nameValuePair n {}) (
        unique (referencedApiKeySecrets ++ identifyingSecretNames)
      ));

    sops.templates."requestrr-settings-json" = {
      content = builtins.toJSON settingsJsonAttrs;
      mode = "0400";
    };

    virtualisation.oci-containers.containers.requestrr = {
      image = imageLib.renderImage cfg.image;
      inherit (cfg) dependsOn;
      environment = {
        "TZ" = cfg.timezone;
      };
      volumes = [
        "${cfg.baseDir}:/root/config"
      ];
      ports = optionals (!cfg.useGluetun) portMappings;
      extraOptions = netOpts;
      log-driver = "journald";
    };

    systemd.services."podman-requestrr" = mkMerge [
      (ociLib.mkServiceConfig {
        networks =
          if cfg.useGluetun
          then []
          else cfg.networks;
        extraAfter = gluetunDeps;
        extraRequires = gluetunDeps;
        sopsTemplates = ["requestrr-settings-json"];
      })
      {
        serviceConfig.ExecStartPre = ["${configInitScript}"];
      }
    ];

    modules.linux.oci._managedPaths.${cfg.baseDir}.properties = cfg.configProperties;
    modules.linux.oci._gluetunPorts = mkIf cfg.useGluetun portMappings;

    modules.linux.oci.networks = mkIf (!cfg.useGluetun) (
      listToAttrs (map (n: nameValuePair n {enable = true;}) cfg.networks)
    );
  });
}
