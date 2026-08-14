{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.calibre;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.calibre = {
    enable = mkEnableOption "Calibre e-book manager (linuxserver/calibre Kasm build)";

    image = imageLib.mkImageOptions {
      repository = "lscr.io/linuxserver/calibre";
      version = "latest";
    };

    baseDir = mkOption {
      description = "Base directory for calibre state (mounted at /config).";
      type = types.str;
      example = "/data/apps/calibre";
    };

    booksDir = mkOption {
      description = "Host directory holding the calibre library (mounted at /books).";
      type = types.str;
      example = "/data/books";
    };

    httpPort = mkOption {
      description = "Host port for the Kasm/HTTP web UI (container 8080).";
      type = types.port;
      default = 8080;
    };

    httpsPort = mkOption {
      description = "Host port for the Kasm/HTTPS web UI (container 8181).";
      type = types.port;
      default = 3229;
    };

    contentPort = mkOption {
      description = "Host port for the calibre content server (container 8081).";
      type = types.port;
      default = 8081;
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

    networks = mkOption {
      description = "Networks to join.";
      type = types.listOf types.str;
      default = ["default"];
    };

    dependsOn = mkOption {
      description = "Other oci-containers this service depends on.";
      type = types.listOf types.str;
      default = [];
    };

    configProperties = mkOption {
      description = "ZFS properties applied to the baseDir dataset. Defaults tuned for SQLite.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };
  };

  config = mkIf cfg.enable (let
    arr = ociLib.mkArrService {
      name = "calibre";
      image = imageLib.renderImage cfg.image;
      extraOptions = imageLib.mkImageLabels {
        module = "calibre";
        inherit (cfg) image;
      };
      inherit (cfg) baseDir;
      inherit (cfg) configProperties;
      mediaMounts = ["${cfg.booksDir}:/books:rw"];
      inherit (cfg) networks user timezone dependsOn;
      environmentFiles = [config.sops.templates."calibre-env".path];
      ports = [
        "${toString cfg.httpPort}:8080"
        "${toString cfg.httpsPort}:8181"
        "${toString cfg.contentPort}:8081"
      ];
      sopsTemplates = ["calibre-env"];
    };
  in {
    sops.secrets."calibre/password" = {};

    sops.templates."calibre-env".content = ''
      PASSWORD=${config.sops.placeholder."calibre/password"}
    '';

    virtualisation.oci-containers.containers.calibre = arr.container;
    systemd.services."podman-calibre" = arr.serviceConfig;
    modules.linux.oci._managedPaths = arr.managedPaths;
    modules.linux.oci.networks = arr.networks;
  });
}
