{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.linkding;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.linkding = {
    enable = mkEnableOption "Linkding bookmark manager";

    image = imageLib.mkImageOptions {
      repository = "sissbruecker/linkding";
      version = "latest";
    };

    baseDir = mkOption {
      description = ''
        Base directory for linkding state (mounted at /etc/linkding/data
        inside the container).
      '';
      type = types.str;
      example = "/data/apps/linkding";
    };

    webPort = mkOption {
      description = "Host port for the linkding web UI.";
      type = types.port;
      default = 7904;
    };

    superuserName = mkOption {
      description = "Initial superuser username (consumed only on first boot).";
      type = types.str;
      default = "ryan";
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

    oidc = {
      enable = mkEnableOption ''
        OIDC authentication. When true the module requires sops secrets at
        "linkding/oidc-client-id" and "linkding/oidc-client-secret"
      '';

      authorizationEndpoint = mkOption {
        description = "OIDC authorization endpoint URL.";
        type = types.str;
        example = "https://auth.example.com/authorize";
      };

      tokenEndpoint = mkOption {
        description = "OIDC token endpoint URL.";
        type = types.str;
        example = "https://auth.example.com/api/oidc/token";
      };

      userEndpoint = mkOption {
        description = "OIDC userinfo endpoint URL.";
        type = types.str;
        example = "https://auth.example.com/api/oidc/userinfo";
      };

      jwksEndpoint = mkOption {
        description = "OIDC JWKS endpoint URL.";
        type = types.str;
        example = "https://auth.example.com/.well-known/jwks.json";
      };

      usePkce = mkOption {
        description = "Whether to use PKCE in the OIDC flow.";
        type = types.bool;
        default = false;
      };

      verifySsl = mkOption {
        description = "Whether to verify the OIDC provider's TLS cert.";
        type = types.bool;
        default = true;
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

    sops.secrets =
      {
        "linkding/superuser-password" = {};
      }
      // optionalAttrs cfg.oidc.enable {
        "linkding/oidc-client-id" = {};
        "linkding/oidc-client-secret" = {};
      };

    sops.templates =
      {
        "linkding-superuser-env".content = ''
          LD_SUPERUSER_PASSWORD=${config.sops.placeholder."linkding/superuser-password"}
        '';
      }
      // optionalAttrs cfg.oidc.enable {
        "linkding-oidc-env".content = ''
          OIDC_RP_CLIENT_ID=${config.sops.placeholder."linkding/oidc-client-id"}
          OIDC_RP_CLIENT_SECRET=${config.sops.placeholder."linkding/oidc-client-secret"}
        '';
      };

    virtualisation.oci-containers.containers.linkding = {
      image = imageLib.renderImage cfg.image;
      inherit (cfg) dependsOn;
      environment =
        {
          "LD_SUPERUSER_NAME" = cfg.superuserName;
        }
        // optionalAttrs cfg.oidc.enable {
          "LD_ENABLE_OIDC" = "True";
          "OIDC_OP_AUTHORIZATION_ENDPOINT" = cfg.oidc.authorizationEndpoint;
          "OIDC_OP_TOKEN_ENDPOINT" = cfg.oidc.tokenEndpoint;
          "OIDC_OP_USER_ENDPOINT" = cfg.oidc.userEndpoint;
          "OIDC_OP_JWKS_ENDPOINT" = cfg.oidc.jwksEndpoint;
          "OIDC_USE_PKCE" =
            if cfg.oidc.usePkce
            then "True"
            else "False";
          "OIDC_VERIFY_SSL" =
            if cfg.oidc.verifySsl
            then "True"
            else "False";
        };
      environmentFiles =
        [config.sops.templates."linkding-superuser-env".path]
        ++ optional cfg.oidc.enable config.sops.templates."linkding-oidc-env".path;
      volumes = [
        "${cfg.baseDir}:/etc/linkding/data"
      ];
      ports = ["${toString cfg.webPort}:9090"];
      extraOptions =
        ["--network-alias=linkding"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ imageLib.mkImageLabels {
          module = "linkding";
          inherit (cfg) image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-linkding" = ociLib.mkServiceConfig {
      inherit (cfg) networks;
      sopsTemplates =
        ["linkding-superuser-env"]
        ++ optional cfg.oidc.enable "linkding-oidc-env";
    };
  };
}
