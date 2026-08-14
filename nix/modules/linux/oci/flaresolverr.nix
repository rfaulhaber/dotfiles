{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.flaresolverr;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.flaresolverr = {
    enable = mkEnableOption "FlareSolverr Cloudflare bypass proxy";

    image = imageLib.mkImageOptions {
      repository = "ghcr.io/flaresolverr/flaresolverr";
      version = "latest";
    };

    webPort = mkOption {
      description = "Host port for the FlareSolverr API (forwarded onto gluetun when useGluetun = true).";
      type = types.port;
      default = 8191;
    };

    logLevel = mkOption {
      description = "FlareSolverr log level.";
      type = types.enum ["debug" "info" "warning" "error"];
      default = "info";
    };

    logHtml = mkOption {
      description = "Whether to dump fetched HTML to logs (debugging only — extremely noisy).";
      type = types.bool;
      default = false;
    };

    captchaSolver = mkOption {
      description = "Captcha solver implementation.";
      type = types.enum ["none" "hcaptcha-solver"];
      default = "none";
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
        gluetun publishes the API port to the host instead. Strongly
        recommended since FlareSolverr's purpose is to issue browser-fingerprinted
        requests on behalf of prowlarr — the source IP is identifying.
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
  };

  config = mkIf cfg.enable (let
    portMappings = ["${toString cfg.webPort}:8191"];
    netOpts =
      (
        if cfg.useGluetun
        then ["--network=container:${cfg.gluetunContainer}"]
        else
          ["--network-alias=flaresolverr"]
          ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
      )
      ++ imageLib.mkImageLabels {
        module = "flaresolverr";
        inherit (cfg) image;
      };
    gluetunDeps = optional cfg.useGluetun "podman-${cfg.gluetunContainer}.service";
  in {
    virtualisation.oci-containers.containers.flaresolverr = {
      image = imageLib.renderImage cfg.image;
      inherit (cfg) dependsOn;
      environment = {
        "LOG_LEVEL" = cfg.logLevel;
        "LOG_HTML" =
          if cfg.logHtml
          then "true"
          else "false";
        "CAPTCHA_SOLVER" = cfg.captchaSolver;
        "TZ" = cfg.timezone;
      };
      ports = optionals (!cfg.useGluetun) portMappings;
      extraOptions = netOpts;
      log-driver = "journald";
    };

    systemd.services."podman-flaresolverr" = ociLib.mkServiceConfig {
      networks =
        if cfg.useGluetun
        then []
        else cfg.networks;
      extraAfter = gluetunDeps;
      extraRequires = gluetunDeps;
    };

    modules.linux.oci._gluetunPorts = mkIf cfg.useGluetun portMappings;

    modules.linux.oci.networks = mkIf (!cfg.useGluetun) (
      listToAttrs (map (n: nameValuePair n {enable = true;}) cfg.networks)
    );
  });
}
