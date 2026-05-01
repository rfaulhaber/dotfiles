{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.filebrowser;
  ociLib = config.modules.linux.oci.lib;
in {
  options.modules.linux.oci.services.filebrowser = {
    enable = mkEnableOption "Filebrowser web file manager";

    image = mkOption {
      description = "Filebrowser container image.";
      type = types.str;
      default = "gtstef/filebrowser:latest";
    };

    baseDir = mkOption {
      description = ''
        Base directory for filebrowser config / database (mounted at
        /home/filebrowser/data inside the container, where filebrowser
        stores its config.yml and SQLite DB).
      '';
      type = types.str;
      example = "/data/apps/filebrowser";
    };

    filesDir = mkOption {
      description = ''
        Host directory exposed to the user via the filebrowser UI
        (mounted at /home/filebrowser/files).
      '';
      type = types.str;
      example = "/data/filebrowser/files";
    };

    webPort = mkOption {
      description = "Host port for the filebrowser web UI.";
      type = types.port;
      default = 6572;
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

    extraEnv = mkOption {
      description = "Additional environment variables.";
      type = types.attrsOf types.str;
      default = {
        FILEBROWSER_CONFIG = "data/config.yml";
      };
    };

    configProperties = mkOption {
      description = "ZFS properties applied to the baseDir dataset. Defaults tuned for SQLite.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci._managedPaths.${cfg.baseDir}.properties = cfg.configProperties;

    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    sops.secrets."filebrowser/admin-password" = {};

    sops.templates."filebrowser-env".content = ''
      FILEBROWSER_ADMIN_PASSWORD=${config.sops.placeholder."filebrowser/admin-password"}
    '';

    virtualisation.oci-containers.containers.filebrowser = {
      image = cfg.image;
      inherit (cfg) dependsOn;
      environment =
        {
          "PUID" = toString cfg.user.uid;
          "PGID" = toString cfg.user.gid;
          "TZ" = cfg.timezone;
        }
        // cfg.extraEnv;
      environmentFiles = [config.sops.templates."filebrowser-env".path];
      volumes = [
        "${cfg.filesDir}:/home/filebrowser/files"
        "${cfg.baseDir}:/home/filebrowser/data"
      ];
      ports = ["${toString cfg.webPort}:80"];
      extraOptions =
        ["--network-alias=filebrowser"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks);
      log-driver = "journald";
    };

    systemd.services."podman-filebrowser" = ociLib.mkServiceConfig {
      networks = cfg.networks;
    };
  };
}
