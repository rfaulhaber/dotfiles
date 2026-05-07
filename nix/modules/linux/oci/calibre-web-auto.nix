{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.calibre-web-auto;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.calibre-web-auto = {
    enable = mkEnableOption "Calibre-Web-Automated";

    image = imageLib.mkImageOptions {
      repository = "crocodilestick/calibre-web-automated";
      version = "latest";
    };

    baseDir = mkOption {
      description = "Base directory for calibre-web-auto state (mounted at /config).";
      type = types.str;
      example = "/data/apps/calibre-web";
    };

    libraryDir = mkOption {
      description = "Host directory holding the calibre library (mounted at /calibre-library).";
      type = types.str;
      example = "/data/books";
    };

    ingestDir = mkOption {
      description = ''
        Host directory used as a drop folder. Files added here are imported
        into the library and removed (mounted at /cwa-book-ingest).
      '';
      type = types.str;
      example = "/data/books/cwa-book-ingest";
    };

    webPort = mkOption {
      description = "Host port for the web UI.";
      type = types.port;
      default = 8089;
    };

    cwaPort = mkOption {
      description = "Internal port the application listens on (CWA_PORT_OVERRIDE).";
      type = types.port;
      default = 8083;
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
      default = ["calibre"];
    };

    networkShareMode = mkOption {
      description = ''
        If true, disables WAL on the SQLite metadata DB to reduce locking
        issues when the library lives on an NFS/SMB share.
      '';
      type = types.bool;
      default = false;
    };

    trustedProxyCount = mkOption {
      description = "Number of reverse proxies in front of the app (TRUSTED_PROXY_COUNT).";
      type = types.int;
      default = 2;
    };

    oauthlibInsecureTransport = mkOption {
      description = "Set OAUTHLIB_INSECURE_TRANSPORT=1 (allows OAuth flows over plain HTTP behind a TLS-terminating proxy).";
      type = types.bool;
      default = true;
    };

    extraEnv = mkOption {
      description = "Additional environment variables.";
      type = types.attrsOf types.str;
      default = {};
    };

    hardcover.enable = mkEnableOption ''
      Hardcover metadata provider integration. When true, the module
      requires a sops secret at "calibre-web-auto/hardcover-token" —
      Hardcover ties tokens to user accounts, so the token is identifying
    '';

    configProperties = mkOption {
      description = "ZFS properties applied to the baseDir dataset. Defaults tuned for SQLite.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };
  };

  config = mkIf cfg.enable (let
    arr = ociLib.mkArrService {
      name = "calibre-web-auto";
      image = imageLib.renderImage cfg.image;
      extraOptions = imageLib.mkImageLabels {
        module = "calibre-web-auto";
        image = cfg.image;
      };
      baseDir = cfg.baseDir;
      configProperties = cfg.configProperties;
      mediaMounts = [
        "${cfg.ingestDir}:/cwa-book-ingest:rw"
        "${cfg.libraryDir}:/calibre-library:rw"
      ];
      inherit (cfg) networks user timezone dependsOn;
      capAdd = ["NET_BIND_SERVICE"];
      extraEnv =
        {
          NETWORK_SHARE_MODE =
            if cfg.networkShareMode
            then "true"
            else "false";
          CWA_PORT_OVERRIDE = toString cfg.cwaPort;
          TRUSTED_PROXY_COUNT = toString cfg.trustedProxyCount;
        }
        // optionalAttrs cfg.oauthlibInsecureTransport {
          OAUTHLIB_INSECURE_TRANSPORT = "1";
        }
        // cfg.extraEnv;
      environmentFiles = optional cfg.hardcover.enable config.sops.templates."calibre-web-auto-env".path;
      ports = ["${toString cfg.webPort}:${toString cfg.cwaPort}"];
    };
  in {
    sops.secrets = mkIf cfg.hardcover.enable {
      "calibre-web-auto/hardcover-token" = {};
    };

    sops.templates = mkIf cfg.hardcover.enable {
      "calibre-web-auto-env".content = ''
        HARDCOVER_TOKEN=${config.sops.placeholder."calibre-web-auto/hardcover-token"}
      '';
    };

    virtualisation.oci-containers.containers.calibre-web-auto = arr.container;
    systemd.services."podman-calibre-web-auto" = arr.serviceConfig;
    modules.linux.oci._managedPaths = arr.managedPaths;
    modules.linux.oci.networks = arr.networks;
  });
}
