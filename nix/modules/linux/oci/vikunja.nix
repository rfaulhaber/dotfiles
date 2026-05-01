{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.vikunja;
  ociLib = config.modules.linux.oci.lib;
in {
  options.modules.linux.oci.services.vikunja = {
    enable = mkEnableOption "Vikunja task management";

    image = mkOption {
      description = "Vikunja container image.";
      type = types.str;
      default = "vikunja/vikunja:latest";
    };

    baseDir = mkOption {
      description = ''
        Parent directory for vikunja state. Two children are bind-mounted
        into the container: <baseDir>/files → /app/vikunja/files and
        <baseDir>/db → /db.
      '';
      type = types.str;
      example = "/data/apps/vikunja";
    };

    configFile = mkOption {
      description = ''
        Optional host path to a vikunja config.yml mounted at
        /etc/vikunja/config.yml. Treated as an opaque host path (not
        imported into the nix store), so vikunja sees live edits without
        a rebuild. When null, vikunja is configured entirely via env vars.
      '';
      type = types.nullOr types.str;
      default = null;
      example = "/data/apps/vikunja/config.yml";
    };

    publicUrl = mkOption {
      description = "VIKUNJA_SERVICE_PUBLICURL — the externally visible URL for vikunja.";
      type = types.str;
      example = "https://tasks.example.com";
    };

    webPort = mkOption {
      description = "Host port for the vikunja web UI / API.";
      type = types.port;
      default = 7734;
    };

    user = mkOption {
      description = ''
        --user spec to run the container as. Vikunja's image expects to
        write to its bind-mounted volumes; the upstream compose runs as
        "0:0" to avoid permission friction with the SQLite DB.
      '';
      type = types.str;
      default = "0:0";
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
      description = "Additional VIKUNJA_* environment variables.";
      type = types.attrsOf types.str;
      default = {};
    };

    filesProperties = mkOption {
      description = "ZFS properties applied to the files dataset.";
      type = types.attrsOf types.str;
      default = {};
    };

    dbProperties = mkOption {
      description = "ZFS properties applied to the db dataset. Defaults tuned for SQLite.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci._managedPaths = {
      # Parent dataset has no mountpoint — only its children are mounted.
      "${cfg.baseDir}".properties.mountpoint = "none";
      "${cfg.baseDir}/files".properties = cfg.filesProperties;
      "${cfg.baseDir}/db".properties = cfg.dbProperties;
    };

    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    sops.secrets."vikunja/jwt-secret" = {};

    sops.templates."vikunja-env".content = ''
      VIKUNJA_SERVICE_JWTSECRET=${config.sops.placeholder."vikunja/jwt-secret"}
    '';

    virtualisation.oci-containers.containers.vikunja = {
      image = cfg.image;
      inherit (cfg) dependsOn;
      environment =
        {
          "VIKUNJA_SERVICE_PUBLICURL" = cfg.publicUrl;
        }
        // cfg.extraEnv;
      environmentFiles = [config.sops.templates."vikunja-env".path];
      volumes =
        [
          "${cfg.baseDir}/files:/app/vikunja/files"
          "${cfg.baseDir}/db:/db"
        ]
        ++ optional (cfg.configFile != null) "${cfg.configFile}:/etc/vikunja/config.yml";
      ports = ["${toString cfg.webPort}:3456"];
      extraOptions =
        ["--network-alias=vikunja"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ ["--user=${cfg.user}"];
      log-driver = "journald";
    };

    systemd.services."podman-vikunja" = ociLib.mkServiceConfig {
      networks = cfg.networks;
    };
  };
}
