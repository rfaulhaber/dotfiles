{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.recyclarr;
  ociLib = config.modules.linux.oci.lib;
in {
  options.modules.linux.oci.services.recyclarr = {
    enable = mkEnableOption "Recyclarr TRaSH-Guides quality profile sync";

    image = mkOption {
      description = "Recyclarr container image.";
      type = types.str;
      default = "ghcr.io/recyclarr/recyclarr:latest";
    };

    baseDir = mkOption {
      description = ''
        Base directory for recyclarr config (mounted at /config). Holds
        recyclarr.yml plus the cached TRaSH-Guides repo and run logs.
      '';
      type = types.str;
      example = "/data/apps/recyclarr";
    };

    user = {
      uid = mkOption {
        description = "UID to run the container as.";
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
        Required when recyclarr's targets (sonarr/radarr) live inside
        gluetun — it reaches them at localhost:8989/7878.
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
      example = ["radarr" "sonarr"];
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
        ["--network-alias=recyclarr"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks);
    gluetunDeps = optional cfg.useGluetun "podman-${cfg.gluetunContainer}.service";
  in {
    virtualisation.oci-containers.containers.recyclarr = {
      image = cfg.image;
      inherit (cfg) dependsOn;
      environment = {
        "TZ" = cfg.timezone;
      };
      volumes = [
        "${cfg.baseDir}:/config"
      ];
      extraOptions =
        netOpts
        ++ [
          "--user=${toString cfg.user.uid}:${toString cfg.user.gid}"
          "--security-opt=no-new-privileges:true"
        ];
      log-driver = "journald";
    };

    systemd.services."podman-recyclarr" = ociLib.mkServiceConfig {
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
