{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.vikunja;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};

  oidcEnabled = cfg.auth.openid.enable && cfg.auth.openid.providers != {};

  providerOpts = {name, ...}: {
    options = {
      displayName = mkOption {
        description = "Human-readable provider name shown on the login button.";
        type = types.str;
        default = name;
      };

      authUrl = mkOption {
        description = "OIDC authority/issuer URL (no trailing slash).";
        type = types.str;
        example = "https://auth.example.com";
      };

      scope = mkOption {
        description = "Space-separated OAuth2 scopes to request.";
        type = types.str;
        default = "openid profile email";
      };

      usernameFallback = mkOption {
        description = "Whether to fall back to the OIDC username claim when no display name is present.";
        type = types.bool;
        default = true;
      };

      emailFallback = mkOption {
        description = "Whether to fall back to the OIDC email claim when no username is present.";
        type = types.bool;
        default = true;
      };
    };
  };

  # config.yml content, rendered via sops template so the OIDC client
  # secrets (sops placeholders) get substituted at activation time.
  # JSON is a strict subset of YAML — toJSON output is valid YAML input
  # for vikunja's parser, and avoids manual indentation in nix.
  configYamlAttrs = optionalAttrs oidcEnabled {
    auth.openid = {
      enabled = true;
      redirecturl = cfg.auth.openid.redirectUrl;
      providers =
        mapAttrs (name: p: {
          name = p.displayName;
          authurl = p.authUrl;
          clientid = config.sops.placeholder."vikunja/oidc-${name}-client-id";
          clientsecret = config.sops.placeholder."vikunja/oidc-${name}-client-secret";
          scope = p.scope;
          usernamefallback = p.usernameFallback;
          emailfallback = p.emailFallback;
        })
        cfg.auth.openid.providers;
    };
  };
in {
  options.modules.linux.oci.services.vikunja = {
    enable = mkEnableOption "Vikunja task management";

    image = imageLib.mkImageOptions {
      repository = "vikunja/vikunja";
      version = "latest";
    };

    baseDir = mkOption {
      description = ''
        Single state directory for vikunja. Two subdirectories are
        bind-mounted into the container: <baseDir>/files → /app/vikunja/files
        and <baseDir>/db → /db. Both are plain directories under one ZFS
        dataset (vikunja is SQLite-only — no postgres tuning needed, so the
        per-concern dataset split would just be structural overhead).
      '';
      type = types.str;
      example = "/data/apps/vikunja";
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

    properties = mkOption {
      description = ''
        ZFS properties applied to the single baseDir dataset. Defaults
        tuned for SQLite (recordsize=64K).
      '';
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };

    auth.openid = {
      enable = mkEnableOption ''
        OpenID Connect authentication. Each provider declared under
        `providers` requires sops secrets at
        "vikunja/oidc-<name>-client-id" and
        "vikunja/oidc-<name>-client-secret"
      '';

      redirectUrl = mkOption {
        description = ''
          OIDC redirect URL (matches the value registered with the
          provider). Vikunja uses a single global redirect; the provider
          name is part of the path.
        '';
        type = types.str;
        example = "https://tasks.example.com/auth/openid/pocketid";
      };

      providers = mkOption {
        description = ''
          OIDC providers. Attribute keys become provider IDs in vikunja's
          config and source the sops secret paths
          (vikunja/oidc-<name>-client-id, ...-client-secret).
        '';
        type = types.attrsOf (types.submodule providerOpts);
        default = {};
      };
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci._managedPaths = {
      "${cfg.baseDir}".properties = cfg.properties;
    };

    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    sops.secrets =
      {
        "vikunja/jwt-secret" = {};
      }
      // (
        # Per-provider secrets, declared once per provider name.
        listToAttrs (concatLists (mapAttrsToList (name: _: [
            (nameValuePair "vikunja/oidc-${name}-client-id" {})
            (nameValuePair "vikunja/oidc-${name}-client-secret" {})
          ])
          cfg.auth.openid.providers))
      );

    sops.templates =
      {
        "vikunja-env".content = ''
          VIKUNJA_SERVICE_JWTSECRET=${config.sops.placeholder."vikunja/jwt-secret"}
        '';
      }
      // optionalAttrs oidcEnabled {
        "vikunja-config-yml" = {
          content = builtins.toJSON configYamlAttrs;
          # World-readable through the bind mount; vikunja runs as 0:0
          # by default but the file still ends up on /run/secrets-rendered
          # which restricts access at the directory level.
          mode = "0444";
        };
      };

    virtualisation.oci-containers.containers.vikunja = {
      image = imageLib.renderImage cfg.image;
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
        ++ optional oidcEnabled
        "${config.sops.templates."vikunja-config-yml".path}:/etc/vikunja/config.yml:ro";
      ports = ["${toString cfg.webPort}:3456"];
      extraOptions =
        ["--network-alias=vikunja"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ ["--user=${cfg.user}"]
        ++ imageLib.mkImageLabels {
          module = "vikunja";
          image = cfg.image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-vikunja" = ociLib.mkServiceConfig {
      networks = cfg.networks;
      sopsTemplates =
        ["vikunja-env"]
        ++ optional oidcEnabled "vikunja-config-yml";
    };
  };
}
