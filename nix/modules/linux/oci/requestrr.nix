{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.requestrr;
  ociLib = config.modules.linux.oci.lib;
in {
  options.modules.linux.oci.services.requestrr = {
    enable = mkEnableOption "Requestrr chat-bot media-request frontend";

    image = mkOption {
      description = "Requestrr container image (community fork).";
      type = types.str;
      default = "thomst08/requestrr:latest";
    };

    baseDir = mkOption {
      description = ''
        Base directory for requestrr config (mounted at /root/config inside
        the container — requestrr runs as root and stores Settings.json
        there).
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
  };

  config = mkIf cfg.enable (let
    portMappings = ["${toString cfg.webPort}:4545"];
    netOpts =
      if cfg.useGluetun
      then ["--network=container:${cfg.gluetunContainer}"]
      else
        ["--network-alias=requestrr"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks);
    gluetunDeps = optional cfg.useGluetun "podman-${cfg.gluetunContainer}.service";
  in {
    virtualisation.oci-containers.containers.requestrr = {
      image = cfg.image;
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

    systemd.services."podman-requestrr" = ociLib.mkServiceConfig {
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
