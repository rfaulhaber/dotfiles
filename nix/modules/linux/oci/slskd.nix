{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.slskd;
  ociLib = config.modules.linux.oci.lib;
in {
  options.modules.linux.oci.services.slskd = {
    enable = mkEnableOption "slskd Soulseek client";

    image = mkOption {
      description = "slskd container image.";
      type = types.str;
      default = "slskd/slskd:latest";
    };

    baseDir = mkOption {
      description = ''
        Base directory for slskd state. Mounted at /app inside the container
        (slskd's config layout uses /app, not /config).
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
      description = "Whether to allow editing slskd.yml from the web UI.";
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

    secretsFile = mkOption {
      description = ''
        Path to env file containing slskd secrets. Must include
        SLSKD_SLSK_USERNAME and SLSKD_SLSK_PASSWORD (the Soulseek account),
        and may include any other SLSKD_* secrets (e.g. integrations).
        Use sops.templates to render from sops secrets.
      '';
      type = types.path;
      example = literalExpression ''config.sops.templates."slskd-env".path'';
    };

    configProperties = mkOption {
      description = "ZFS properties applied to the baseDir dataset. Defaults tuned for LiteDB.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };
  };

  config = mkIf cfg.enable (let
    portMappings = [
      "${toString cfg.webPort}:5030"
      "${toString cfg.grpcPort}:5031"
      "${toString cfg.peerPort}:50300"
    ];
    netOpts =
      if cfg.useGluetun
      then ["--network=container:${cfg.gluetunContainer}"]
      else
        ["--network-alias=slskd"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks);
    gluetunDeps = optional cfg.useGluetun "podman-${cfg.gluetunContainer}.service";
  in {
    virtualisation.oci-containers.containers.slskd = {
      image = cfg.image;
      inherit (cfg) dependsOn;
      environment = {
        "SLSKD_REMOTE_CONFIGURATION" =
          if cfg.remoteConfiguration
          then "true"
          else "false";
        "TZ" = cfg.timezone;
      };
      environmentFiles = [cfg.secretsFile];
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

    systemd.services."podman-slskd" = ociLib.mkServiceConfig {
      networks =
        if cfg.useGluetun
        then []
        else cfg.networks;
      extraAfter = gluetunDeps;
      extraRequires = gluetunDeps;
    };

    modules.linux.oci._managedPaths.${cfg.baseDir}.properties = cfg.configProperties;
    modules.linux.oci._gluetunPorts = mkIf cfg.useGluetun portMappings;

    modules.linux.oci.networks = mkIf (!cfg.useGluetun) (
      listToAttrs (map (n: nameValuePair n {enable = true;}) cfg.networks)
    );
  });
}
