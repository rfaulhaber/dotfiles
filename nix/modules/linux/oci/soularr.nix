{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.soularr;
  ociLib = config.modules.linux.oci.lib;
in {
  options.modules.linux.oci.services.soularr = {
    enable = mkEnableOption "Soularr (lidarr → slskd bridge)";

    image = mkOption {
      description = "Soularr container image.";
      type = types.str;
      default = "mrusse08/soularr:latest";
    };

    baseDir = mkOption {
      description = ''
        Base directory for soularr config (mounted at /data inside the
        container — soularr expects its config.ini there, not at /config).
      '';
      type = types.str;
      example = "/data/apps/soularr";
    };

    slskdDownloadsDir = mkOption {
      description = ''
        Host directory containing slskd's completed downloads (mounted at
        /downloads). Soularr scans this to match completed Soulseek
        downloads to lidarr requests. Should match slskd.downloadsDir.
      '';
      type = types.str;
      example = "/data/slskd";
    };

    scriptInterval = mkOption {
      description = "Seconds between soularr scheduler runs.";
      type = types.int;
      default = 300;
    };

    user = {
      uid = mkOption {
        description = "UID to run the container as (soularr uses --user, not PUID).";
        type = types.int;
        default = 1000;
      };
      gid = mkOption {
        description = "GID to run the container as.";
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
        Route through the gluetun VPN container's network namespace.
        Required for soularr in this setup: it reaches slskd and lidarr
        at localhost:<port>, and both of those services live inside the
        gluetun netns.
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
      default = ["slskd" "lidarr"];
    };

    configProperties = mkOption {
      description = "ZFS properties applied to the baseDir dataset.";
      type = types.attrsOf types.str;
      default = {};
    };
  };

  config = mkIf cfg.enable (let
    netOpts =
      if cfg.useGluetun
      then ["--network=container:${cfg.gluetunContainer}"]
      else
        ["--network-alias=soularr"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks);
    gluetunDeps = optional cfg.useGluetun "podman-${cfg.gluetunContainer}.service";
  in {
    virtualisation.oci-containers.containers.soularr = {
      image = cfg.image;
      inherit (cfg) dependsOn;
      environment = {
        "TZ" = cfg.timezone;
        "SCRIPT_INTERVAL" = toString cfg.scriptInterval;
      };
      volumes = [
        "${cfg.slskdDownloadsDir}:/downloads"
        "${cfg.baseDir}:/data"
      ];
      extraOptions =
        netOpts
        ++ ["--user=${toString cfg.user.uid}:${toString cfg.user.gid}"];
      log-driver = "journald";
    };

    systemd.services."podman-soularr" = ociLib.mkServiceConfig {
      networks =
        if cfg.useGluetun
        then []
        else cfg.networks;
      extraAfter = gluetunDeps;
      extraRequires = gluetunDeps;
    };

    modules.linux.oci._managedPaths.${cfg.baseDir}.properties = cfg.configProperties;

    modules.linux.oci.networks = mkIf (!cfg.useGluetun) (
      listToAttrs (map (n: nameValuePair n {enable = true;}) cfg.networks)
    );
  });
}
