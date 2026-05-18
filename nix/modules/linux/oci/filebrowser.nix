{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.filebrowser;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.filebrowser = {
    enable = mkEnableOption "Filebrowser web file manager";

    image = imageLib.mkImageOptions {
      repository = "gtstef/filebrowser";
      version = "latest";
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
      description = ''
        Port for the filebrowser web UI — used for both the host
        publish and the container's internal listen. The container
        runs as non-root and can't bind &lt;1024, so this *must* match
        the `server.port:` setting in baseDir/config.yml. Default
        6572 is a safe high port.
      '';
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

    # ----- config.yml fields rendered via sops template ---------------
    # The whole config is generated from these options as JSON (which is
    # valid YAML) and bind-mounted into the container, so config drift
    # between disk and module is impossible. Secrets (OIDC creds) come
    # from sops via placeholder substitution.

    sources = mkOption {
      description = ''
        Filesystem sources exposed in the UI. Each entry maps a path
        inside the container (under /home/filebrowser/files when using
        the default filesDir mount) to a per-source config block.
      '';
      type = types.listOf types.attrs;
      default = [
        {
          path = "/home/filebrowser/files";
          config = {
            defaultEnabled = true;
            createUserDir = true;
            defaultUserScope = "/";
          };
        }
      ];
    };

    databasePath = mkOption {
      description = "SQLite DB path, relative to filebrowser's working dir inside the container.";
      type = types.str;
      default = "data/database.db";
    };

    userPermissions = mkOption {
      description = "Default permissions granted to newly-created users.";
      type = types.attrsOf types.bool;
      default = {
        api = false;
        admin = false;
        modify = true;
        share = true;
        realtime = true;
        delete = true;
        create = true;
        download = true;
      };
    };

    oidc = {
      enable = mkOption {
        description = ''
          Enable OIDC authentication. When true, requires sops secrets
          at "filebrowser/oidc-client-id" and "filebrowser/oidc-client-secret".
        '';
        type = types.bool;
        default = false;
      };

      issuerUrl = mkOption {
        description = "OIDC issuer URL (no trailing slash).";
        type = types.str;
        example = "https://auth.example.com";
      };

      scopes = mkOption {
        description = "Space-separated OAuth2 scopes to request.";
        type = types.str;
        default = "email openid profile groups";
      };

      userIdentifier = mkOption {
        description = "OIDC claim to use as the filebrowser username.";
        type = types.str;
        default = "preferred_username";
      };

      disableVerifyTLS = mkOption {
        description = "Skip TLS verification for the OIDC issuer (useful for self-signed dev).";
        type = types.bool;
        default = false;
      };

      createUser = mkOption {
        description = "Auto-create filebrowser users from OIDC sign-ins.";
        type = types.bool;
        default = true;
      };

      adminGroup = mkOption {
        description = "OIDC group whose members get filebrowser admin.";
        type = types.str;
        default = "admin";
      };
    };

    passwordAuth.enable = mkOption {
      description = "Enable username/password auth (independent of OIDC).";
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable (let
    # Build the auth.methods subtree, conditionally including OIDC.
    authMethods =
      {
        password.enabled = cfg.passwordAuth.enable;
      }
      // optionalAttrs cfg.oidc.enable {
        oidc = {
          enabled = true;
          # Both clientId and clientSecret are identifying material — kept
          # in sops, never in the nix store. Placeholders get substituted
          # by sops-nix when the template is rendered at activation time.
          clientId = config.sops.placeholder."filebrowser/oidc-client-id";
          clientSecret = config.sops.placeholder."filebrowser/oidc-client-secret";
          issuerUrl = cfg.oidc.issuerUrl;
          scopes = cfg.oidc.scopes;
          userIdentifier = cfg.oidc.userIdentifier;
          disableVerifyTLS = cfg.oidc.disableVerifyTLS;
          createUser = cfg.oidc.createUser;
          adminGroup = cfg.oidc.adminGroup;
        };
      };

    # JSON is a strict subset of YAML, so toJSON output is valid YAML
    # input for filebrowser. Avoids manual indentation / quoting in nix.
    configContent = builtins.toJSON {
      server = {
        port = cfg.webPort;
        sources = cfg.sources;
        database = cfg.databasePath;
      };
      userDefaults = {
        permissions = cfg.userPermissions;
      };
      auth = {
        methods = authMethods;
      };
    };
  in {
    modules.linux.oci._managedPaths.${cfg.baseDir}.properties = cfg.configProperties;

    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    sops.secrets =
      {
        "filebrowser/admin-password" = {};
      }
      // optionalAttrs cfg.oidc.enable {
        "filebrowser/oidc-client-id" = {};
        "filebrowser/oidc-client-secret" = {};
      };

    sops.templates = {
      "filebrowser-env".content = ''
        FILEBROWSER_ADMIN_PASSWORD=${config.sops.placeholder."filebrowser/admin-password"}
      '';
      "filebrowser-config" = {
        content = configContent;
        # World-readable so the non-root container user (uid 1000) can
        # read it through the bind mount. The file lives under
        # /run/secrets/ which is itself only traversable by members of
        # the keys group + container runtimes podman exposes.
        mode = "0444";
      };
    };

    virtualisation.oci-containers.containers.filebrowser = {
      image = imageLib.renderImage cfg.image;
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
        # Bind-mount the rendered config.yml on top of the baseDir
        # mount so the on-disk config.yml in baseDir is shadowed by
        # the nix-rendered one. Read-only.
        "${config.sops.templates."filebrowser-config".path}:/home/filebrowser/data/config.yml:ro"
      ];
      ports = ["${toString cfg.webPort}:${toString cfg.webPort}"];
      extraOptions =
        ["--network-alias=filebrowser"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ imageLib.mkImageLabels {
          module = "filebrowser";
          image = cfg.image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-filebrowser" = ociLib.mkServiceConfig {
      networks = cfg.networks;
      sopsTemplates = ["filebrowser-config" "filebrowser-env"];
    };
  });
}
