{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.shelfmark;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.shelfmark = {
    enable = mkEnableOption "Shelfmark book and audiobook downloader";

    image = imageLib.mkImageOptions {
      repository = "ghcr.io/calibrain/shelfmark";
      version = "latest";
    };

    baseDir = mkOption {
      description = ''
        Base directory for shelfmark state (mounted at /config): the settings
        JSON files, the users SQLite database and the cover cache. Settings
        supplied through the environment are copied into these files in
        plaintext on every start.
      '';
      type = types.str;
      example = "/data/apps/shelfmark";
    };

    ingestDir = mkOption {
      description = ''
        Host directory finished downloads are delivered to (mounted at
        /books). Point it at a library's drop folder, such as calibre's
        auto-add folder, to have downloads imported. Calibre only scans the
        top level of that folder, so keep the default "rename" file
        organization rather than "organize", which writes into
        per-author subfolders.
      '';
      type = types.str;
      example = "/data/import/calibre";
    };

    calibreLibraryDir = mkOption {
      description = ''
        Host directory of a calibre library, mounted read-only at
        /calibre-library to mark search results already in it. The folder is
        mounted rather than metadata.db alone so SQLite sees the -wal/-shm
        sidecars of a library that is being written to.
      '';
      type = types.nullOr types.str;
      default = null;
      example = "/data/books";
    };

    webPort = mkOption {
      description = "Host port for the web UI (forwarded onto gluetun when useGluetun = true).";
      type = types.port;
      default = 8084;
    };

    authMethod = mkOption {
      description = ''
        AUTH_METHOD. Set from the environment, which locks it against changes
        in the UI. "builtin" never falls back to open access when no local
        admin exists, so a fresh install has no one who can sign in: start it
        once with "none", create the admin under Settings > Users, then
        switch back.
      '';
      type = types.enum ["none" "builtin"];
      default = "builtin";
    };

    user = {
      uid = mkOption {
        description = "UID the app drops to (PUID); owns delivered files.";
        type = types.int;
        default = 1000;
      };
      gid = mkOption {
        description = "GID the app drops to (PGID); owns delivered files.";
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
        gluetun publishes the web port to the host instead. Recommended:
        shadow-library mirrors and the bundled Chromium Cloudflare bypass
        otherwise reach out from the host's own address.
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

    extraVolumes = mkOption {
      description = ''
        Additional bind mounts in podman's host:container[:options] form.
        Shelfmark reads a download client's completed files at the path the
        client reports, so mount its download directory at the same
        container path the client itself uses.
      '';
      type = types.listOf types.str;
      default = [];
      example = ["/data/transmission:/downloads:ro"];
    };

    extraEnv = mkOption {
      description = "Additional environment variables (any setting key from the upstream env reference).";
      type = types.attrsOf types.str;
      default = {};
    };

    configProperties = mkOption {
      description = "ZFS properties applied to the baseDir dataset. Defaults tuned for SQLite.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };
  };

  config = mkIf cfg.enable (let
    portMappings = ["${toString cfg.webPort}:8084"];
    arr = ociLib.mkArrService {
      name = "shelfmark";
      image = imageLib.renderImage cfg.image;
      extraOptions = imageLib.mkImageLabels {
        module = "shelfmark";
        inherit (cfg) image;
      };
      inherit (cfg) baseDir configProperties;
      mediaMounts =
        ["${cfg.ingestDir}:/books:rw"]
        ++ optional (cfg.calibreLibraryDir != null) "${cfg.calibreLibraryDir}:/calibre-library:ro"
        ++ cfg.extraVolumes;
      inherit (cfg) useGluetun gluetunContainer networks user timezone dependsOn;
      extraEnv =
        {AUTH_METHOD = cfg.authMethod;}
        // optionalAttrs (cfg.calibreLibraryDir != null) {
          LIBRARY_CHECK_CALIBRE_ENABLED = "true";
        }
        // cfg.extraEnv;
      ports = portMappings;
      gluetunPorts = portMappings;
    };
  in {
    virtualisation.oci-containers.containers.shelfmark = arr.container;
    systemd.services."podman-shelfmark" = arr.serviceConfig;
    modules.linux.oci._managedPaths = arr.managedPaths;
    modules.linux.oci._gluetunPorts = arr.gluetunPorts;
    modules.linux.oci.networks = arr.networks;
  });
}
