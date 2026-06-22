{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.kitchenowl;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};

  oidcEnabled = cfg.oidc.enable;
in {
  options.modules.linux.oci.services.kitchenowl = {
    enable = mkEnableOption "KitchenOwl grocery list and recipe manager";

    # All-in-one image: bundles the Flutter web frontend and the Python
    # backend behind one uWSGI process on port 8080. SQLite by default
    # (STORAGE_PATH=/data), so a single ZFS dataset is all this needs.
    image = imageLib.mkImageOptions {
      repository = "tombursch/kitchenowl";
      version = "latest";
    };

    baseDir = mkOption {
      description = ''
        State directory for KitchenOwl, bind-mounted to the container's
        /data (STORAGE_PATH). Holds the SQLite database and uploads.
      '';
      type = types.str;
      example = "/data/apps/kitchenowl";
    };

    frontUrl = mkOption {
      description = ''
        FRONT_URL — the externally visible base URL. Required for correct
        link generation behind the reverse proxy and for the OIDC redirect
        URIs. No trailing slash.
      '';
      type = types.str;
      example = "https://kitchenowl.example.com";
    };

    webPort = mkOption {
      description = ''
        Host port mapped to the container's 8080. Only needed for direct
        LAN access; external traffic arrives via the newt/Pangolin tunnel
        by container alias on the internal port.
      '';
      type = types.port;
      default = 8094;
    };

    timezone = mkOption {
      description = "TZ for the container.";
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
      description = "Additional environment variables for the container.";
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

    oidc = {
      enable = mkEnableOption ''
        OpenID Connect SSO. Requires sops secrets at
        "kitchenowl/oidc-client-id" and "kitchenowl/oidc-client-secret".
        Register two redirect URIs with the provider:
        "<frontUrl>/signin/redirect" and "kitchenowl:/signin/redirect"
      '';

      issuer = mkOption {
        description = ''
          OIDC_ISSUER — the provider's issuer URL. KitchenOwl performs
          discovery against <issuer>/.well-known/openid-configuration.
          For Pocket ID this is just its base app URL.
        '';
        type = types.str;
        default = "https://auth.3679.space";
      };

      rfcCompliantRedirect = mkOption {
        description = ''
          OIDC_RFC_COMPLIANT_REDIRECT. When false, KitchenOwl uses the
          legacy mobile redirect form (kitchenowl:///signin/redirect).
          Leave true unless an older mobile client requires otherwise.
        '';
        type = types.bool;
        default = true;
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
        "kitchenowl/jwt-secret" = {};
      }
      // optionalAttrs oidcEnabled {
        "kitchenowl/oidc-client-id" = {};
        "kitchenowl/oidc-client-secret" = {};
      };

    # JWT_SECRET_KEY and the OIDC client credentials are rendered into the
    # env file so the sops placeholders are substituted at activation time.
    # podman does not expand $VAR in env values, so anything secret has to
    # arrive pre-substituted here rather than in the inline environment.
    sops.templates."kitchenowl-env".content =
      ''
        JWT_SECRET_KEY=${config.sops.placeholder."kitchenowl/jwt-secret"}
      ''
      + optionalString oidcEnabled ''
        OIDC_CLIENT_ID=${config.sops.placeholder."kitchenowl/oidc-client-id"}
        OIDC_CLIENT_SECRET=${config.sops.placeholder."kitchenowl/oidc-client-secret"}
      '';

    virtualisation.oci-containers.containers.kitchenowl = {
      image = imageLib.renderImage cfg.image;
      inherit (cfg) dependsOn;
      environment =
        {
          "TZ" = cfg.timezone;
          "FRONT_URL" = cfg.frontUrl;
        }
        // optionalAttrs oidcEnabled {
          "OIDC_ISSUER" = cfg.oidc.issuer;
          "OIDC_RFC_COMPLIANT_REDIRECT" =
            if cfg.oidc.rfcCompliantRedirect
            then "True"
            else "False";
        }
        // cfg.extraEnv;
      environmentFiles = [config.sops.templates."kitchenowl-env".path];
      volumes = ["${cfg.baseDir}:/data"];
      ports = ["${toString cfg.webPort}:8080"];
      extraOptions =
        ["--network-alias=kitchenowl"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ imageLib.mkImageLabels {
          module = "kitchenowl";
          image = cfg.image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-kitchenowl" = ociLib.mkServiceConfig {
      networks = cfg.networks;
      sopsTemplates = ["kitchenowl-env"];
    };
  };
}
