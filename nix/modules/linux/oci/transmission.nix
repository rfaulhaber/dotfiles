{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.transmission;
  ociLib = config.modules.linux.oci.lib;

  # transmission accepts a partial settings.json — it fills in built-in
  # defaults for missing keys. So we only have to write the keys we
  # actually care about, plus what the user has explicitly tuned.
  settingsJsonAttrs =
    {
      "download-dir" = cfg.downloadDir;
      "incomplete-dir" = cfg.incompleteDir;
      "incomplete-dir-enabled" = cfg.incompleteDirEnabled;
      "peer-port" = cfg.peerPort;
      "peer-port-random-on-start" = cfg.peerPortRandomOnStart;
      "port-forwarding-enabled" = cfg.portForwardingEnabled;
      "ratio-limit" = cfg.ratioLimit;
      "ratio-limit-enabled" = cfg.ratioLimitEnabled;
      "idle-seeding-limit" = cfg.idleSeedingLimit;
      "idle-seeding-limit-enabled" = cfg.idleSeedingLimitEnabled;
      "peer-limit-global" = cfg.peerLimits.global;
      "peer-limit-per-torrent" = cfg.peerLimits.perTorrent;
      "upload-slots-per-torrent" = cfg.uploadSlotsPerTorrent;
      "umask" = cfg.umask;
      "watch-dir" = cfg.watchDir;
      "watch-dir-enabled" = cfg.watchDirEnabled;
      "rpc-port" = cfg.rpcPort;
      "rpc-url" = cfg.rpcUrl;
      # rpc-username is injected by linuxserver/transmission's init script
      # from the TRANSMISSION_USER env var on first start, so we leave it
      # out of settings.json — keeps the username out of the nix store.
      "queue-stalled-enabled" = cfg.queueStalledEnabled;
      "queue-stalled-minutes" = cfg.queueStalledMinutes;
      "download-queue-enabled" = cfg.downloadQueueEnabled;
      "download-queue-size" = cfg.downloadQueueSize;
    }
    // cfg.extraSettings;

  settingsJsonFile =
    pkgs.writeText "transmission-settings.json"
    (builtins.toJSON settingsJsonAttrs);
in {
  options.modules.linux.oci.services.transmission = {
    enable = mkEnableOption "Transmission BitTorrent client";

    image = mkOption {
      description = "Transmission container image.";
      type = types.str;
      default = "lscr.io/linuxserver/transmission:latest";
    };

    baseDir = mkOption {
      description = ''
        Base directory for transmission state. settings.json is copied in
        from the nix store at boot via ExecStartPre — transmission
        rewrites the file on shutdown, so a read-only bind mount would
        cause errors.
      '';
      type = types.str;
      example = "/data/apps/transmission";
    };

    downloadsDir = mkOption {
      description = "Host directory for completed downloads (mounted at /downloads).";
      type = types.str;
      example = "/data/transmission";
    };

    webPort = mkOption {
      description = "Host port for the transmission web UI.";
      type = types.port;
      default = 9091;
    };

    peerPort = mkOption {
      description = "Peer port (BitTorrent peer connections, tcp + udp).";
      type = types.port;
      default = 51413;
    };

    user = {
      uid = mkOption {
        description = "UID inside the container.";
        type = types.int;
        default = 1000;
      };
      gid = mkOption {
        description = "GID inside the container.";
        type = types.int;
        default = 100;
      };
    };

    timezone = mkOption {
      description = "Timezone for the container.";
      type = types.str;
      default = "America/New_York";
    };

    useGluetun = mkOption {
      description = ''
        Route all traffic through the gluetun VPN container by joining its
        network namespace. Disables this container's own port mappings;
        gluetun publishes the web/peer ports to the host instead.
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

    configProperties = mkOption {
      description = "ZFS properties applied to the baseDir dataset.";
      type = types.attrsOf types.str;
      default = {};
    };

    # ----- settings.json fields -----------------------------------------

    downloadDir = mkOption {
      description = "Container path where completed downloads land.";
      type = types.str;
      default = "/downloads/complete";
    };

    incompleteDir = mkOption {
      description = "Container path for partially-downloaded files.";
      type = types.str;
      default = "/downloads/incomplete";
    };

    incompleteDirEnabled = mkOption {
      description = "Use a separate incomplete directory.";
      type = types.bool;
      default = true;
    };

    peerPortRandomOnStart = mkOption {
      description = "Randomize peer port on each transmission start.";
      type = types.bool;
      default = false;
    };

    portForwardingEnabled = mkOption {
      description = "Whether transmission asks the gateway (UPnP/NAT-PMP) to forward the peer port.";
      type = types.bool;
      default = true;
    };

    ratioLimit = mkOption {
      description = "Default seed ratio target (e.g. 2.0).";
      type = types.float;
      default = 2.0;
    };

    ratioLimitEnabled = mkOption {
      description = "Stop seeding after reaching ratioLimit.";
      type = types.bool;
      default = true;
    };

    idleSeedingLimit = mkOption {
      description = "Stop seeding torrents idle for this many minutes (when idleSeedingLimitEnabled).";
      type = types.int;
      default = 30;
    };

    idleSeedingLimitEnabled = mkOption {
      description = "Enable idle-seeding limit.";
      type = types.bool;
      default = true;
    };

    peerLimits = {
      global = mkOption {
        description = "Maximum global peer connections.";
        type = types.int;
        default = 200;
      };
      perTorrent = mkOption {
        description = "Maximum peer connections per torrent.";
        type = types.int;
        default = 50;
      };
    };

    uploadSlotsPerTorrent = mkOption {
      description = "Maximum simultaneous upload slots per torrent.";
      type = types.int;
      default = 14;
    };

    umask = mkOption {
      description = ''
        Umask used by transmission for new files (string form, e.g. "002").
        Note: transmission stores this as a string in settings.json even
        though it's an octal value.
      '';
      type = types.str;
      default = "002";
    };

    watchDir = mkOption {
      description = "Container path of the watched directory for .torrent files.";
      type = types.str;
      default = "/watch";
    };

    watchDirEnabled = mkOption {
      description = "Enable watch-dir auto-add.";
      type = types.bool;
      default = true;
    };

    rpcPort = mkOption {
      description = "Internal port the RPC/web UI listens on.";
      type = types.port;
      default = 9091;
    };

    rpcUrl = mkOption {
      description = "URL prefix for the RPC/web UI.";
      type = types.str;
      default = "/transmission/";
    };

    queueStalledEnabled = mkOption {
      description = "Mark torrents as stalled after queueStalledMinutes of no traffic.";
      type = types.bool;
      default = true;
    };

    queueStalledMinutes = mkOption {
      description = "Minutes of idle peer activity before a torrent counts as stalled.";
      type = types.int;
      default = 30;
    };

    downloadQueueEnabled = mkOption {
      description = "Limit concurrently-active downloads.";
      type = types.bool;
      default = true;
    };

    downloadQueueSize = mkOption {
      description = "Maximum simultaneous active downloads.";
      type = types.int;
      default = 10;
    };

    extraSettings = mkOption {
      description = ''
        Raw key→value overrides folded into settings.json. Keys use
        transmission's own dashed naming (e.g. "alt-speed-down"). Values
        win over the structured options above on conflict — useful for
        fields not covered by an option here.
      '';
      type = types.attrs;
      default = {};
    };
  };

  config = mkIf cfg.enable (let
    portMappings = [
      "${toString cfg.webPort}:9091"
      "${toString cfg.peerPort}:51413/tcp"
      "${toString cfg.peerPort}:51413/udp"
    ];
    arr = ociLib.mkArrService {
      name = "transmission";
      image = cfg.image;
      baseDir = cfg.baseDir;
      configProperties = cfg.configProperties;
      mediaMounts = ["${cfg.downloadsDir}:/downloads"];
      inherit (cfg) useGluetun gluetunContainer networks user timezone;
      environmentFiles = [config.sops.templates."transmission-env".path];
      ports = portMappings;
      gluetunPorts = portMappings;
    };

    configInitScript = pkgs.writeShellScript "transmission-config-init" ''
      install -m 0640 -o ${toString cfg.user.uid} -g ${toString cfg.user.gid} \
        ${settingsJsonFile} \
        ${cfg.baseDir}/settings.json
    '';
  in {
    sops = {
      secrets = {
        "transmission/password" = {};
        "transmission/username" = {};
      };

      templates."transmission-env".content = ''
        USER=${config.sops.placeholder."transmission/username"}
        TRANSMISSION_USER=${config.sops.placeholder."transmission/username"}
        TRANSMISSION_PASS=${config.sops.placeholder."transmission/password"}
      '';
    };

    virtualisation.oci-containers.containers.transmission = arr.container;
    systemd.services."podman-transmission" = mkMerge [
      arr.serviceConfig
      {
        serviceConfig.ExecStartPre = ["${configInitScript}"];
      }
    ];
    modules.linux.oci._managedPaths = arr.managedPaths;
    modules.linux.oci._gluetunPorts = arr.gluetunPorts;
    modules.linux.oci.networks = arr.networks;
  });
}
