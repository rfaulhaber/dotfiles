{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.grafana;
  ociLib = config.modules.linux.oci.lib;
in {
  options.modules.linux.oci.services.grafana = {
    enable = mkEnableOption "Grafana visualization server";

    image = mkOption {
      description = "Grafana container image.";
      type = types.str;
      default = "grafana/grafana:13.0.1";
    };

    baseDir = mkOption {
      description = ''
        State directory for Grafana, mounted at /var/lib/grafana.
        Holds the SQLite DB, plugin installs, and provisioning lock files.
      '';
      type = types.str;
      example = "/data/apps/grafana";
    };

    port = mkOption {
      description = "Host port for the Grafana UI.";
      type = types.port;
      default = 3000;
    };

    openFirewall = mkOption {
      description = "Whether to open the Grafana port in the host firewall for LAN access.";
      type = types.bool;
      default = false;
    };

    rootUrl = mkOption {
      description = ''
        GF_SERVER_ROOT_URL — must match the URL the user's browser uses
        to reach Grafana. Matters for OIDC redirect generation. Include
        the port unless fronted by a reverse proxy.
      '';
      type = types.str;
      example = "http://atlas.lan:3000";
    };

    networks = mkOption {
      description = ''
        Networks to join. Should include the network shared with Loki
        and Prometheus so they can be auto-provisioned as datasources
        by alias (e.g. "observability").
      '';
      type = types.listOf types.str;
      default = ["observability"];
    };

    dependsOn = mkOption {
      description = "Other oci-containers this service depends on.";
      type = types.listOf types.str;
      default = [];
    };

    extraEnv = mkOption {
      description = "Additional environment variables (typically GF_*).";
      type = types.attrsOf types.str;
      default = {};
    };

    user = {
      uid = mkOption {
        description = "UID inside the container. Default 472 matches the official grafana image.";
        type = types.int;
        default = 472;
      };
      gid = mkOption {
        description = "GID inside the container. Default 0 (root group) matches the official grafana image.";
        type = types.int;
        default = 0;
      };
    };

    configProperties = mkOption {
      description = "ZFS properties applied to baseDir. Defaults tuned for Grafana's SQLite DB.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };

    datasources = mkOption {
      description = ''
        Datasources auto-provisioned at start via Grafana's file
        provisioning. Each entry becomes a row in
        /etc/grafana/provisioning/datasources/datasources.yaml.
      '';
      type = types.listOf types.attrs;
      default = [
        {
          name = "Prometheus";
          type = "prometheus";
          access = "proxy";
          url = "http://prometheus:9090";
          isDefault = true;
        }
        {
          name = "Loki";
          type = "loki";
          access = "proxy";
          url = "http://loki:3100";
        }
      ];
    };

    oidc = {
      enable = mkEnableOption ''
        OIDC SSO via GF_AUTH_GENERIC_OAUTH_*. When true, the module
        requires sops secrets at "grafana/oidc-client-id" and
        "grafana/oidc-client-secret".
      '';

      issuerUrl = mkOption {
        description = "OIDC issuer URL (no trailing slash). For PocketID this is the bare auth host.";
        type = types.str;
        example = "https://auth.example.com";
      };

      providerName = mkOption {
        description = "Display name shown on the Grafana login button.";
        type = types.str;
        default = "SSO";
      };

      scopes = mkOption {
        description = "Space-separated OAuth2 scopes to request.";
        type = types.str;
        default = "openid profile email groups";
      };

      allowedGroups = mkOption {
        description = ''
          OIDC groups allowed to log in. Empty list means anyone with
          a valid token from the issuer can sign in.
        '';
        type = types.listOf types.str;
        default = [];
      };

      adminGroup = mkOption {
        description = ''
          OIDC group whose members get the Grafana Admin role. Other
          authenticated users get Viewer. Implemented via the JMESPath
          role_attribute_path expression.
        '';
        type = types.str;
        default = "admin";
      };
    };
  };

  config = mkIf cfg.enable (let
    datasourcesYaml = builtins.toJSON {
      apiVersion = 1;
      datasources = cfg.datasources;
    };
  in {
    modules.linux.oci._managedPaths.${cfg.baseDir}.properties = cfg.configProperties;

    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    sops.secrets =
      {
        "grafana/admin-password" = {};
      }
      // optionalAttrs cfg.oidc.enable {
        "grafana/oidc-client-id" = {};
        "grafana/oidc-client-secret" = {};
      };

    sops.templates =
      {
        "grafana-env".content = ''
          GF_SECURITY_ADMIN_PASSWORD=${config.sops.placeholder."grafana/admin-password"}
        '';
        "grafana-datasources" = {
          # JSON is a strict subset of YAML, so toJSON output parses
          # cleanly through Grafana's YAML provisioning loader without
          # us hand-rolling YAML in nix.
          content = datasourcesYaml;
          # World-readable so the non-root container user (uid 472) can
          # read the bind-mounted file through /run/secrets/.
          mode = "0444";
        };
      }
      // optionalAttrs cfg.oidc.enable {
        "grafana-oidc-env".content = ''
          GF_AUTH_GENERIC_OAUTH_CLIENT_ID=${config.sops.placeholder."grafana/oidc-client-id"}
          GF_AUTH_GENERIC_OAUTH_CLIENT_SECRET=${config.sops.placeholder."grafana/oidc-client-secret"}
        '';
      };

    virtualisation.oci-containers.containers.grafana = {
      image = cfg.image;
      inherit (cfg) dependsOn;
      environment =
        {
          "TZ" = "America/New_York";
          "GF_SERVER_ROOT_URL" = cfg.rootUrl;
          "GF_SECURITY_ADMIN_USER" = "admin";
          # Disable anonymous telemetry; we opt in to nothing by default.
          "GF_ANALYTICS_REPORTING_ENABLED" = "false";
          "GF_ANALYTICS_CHECK_FOR_UPDATES" = "false";
        }
        // optionalAttrs cfg.oidc.enable {
          "GF_AUTH_GENERIC_OAUTH_ENABLED" = "true";
          "GF_AUTH_GENERIC_OAUTH_NAME" = cfg.oidc.providerName;
          "GF_AUTH_GENERIC_OAUTH_AUTH_URL" = "${cfg.oidc.issuerUrl}/authorize";
          "GF_AUTH_GENERIC_OAUTH_TOKEN_URL" = "${cfg.oidc.issuerUrl}/api/oidc/token";
          "GF_AUTH_GENERIC_OAUTH_API_URL" = "${cfg.oidc.issuerUrl}/api/oidc/userinfo";
          "GF_AUTH_GENERIC_OAUTH_SCOPES" = cfg.oidc.scopes;
          # JMESPath: if the user's groups[] contains the admin group,
          # assign GrafanaAdmin; otherwise Viewer. Single quotes are
          # JMESPath literal strings, NOT shell quoting.
          "GF_AUTH_GENERIC_OAUTH_ROLE_ATTRIBUTE_PATH" = "contains(groups[*], '${cfg.oidc.adminGroup}') && 'GrafanaAdmin' || 'Viewer'";
          "GF_AUTH_GENERIC_OAUTH_ALLOW_ASSIGN_GRAFANA_ADMIN" = "true";
          "GF_AUTH_GENERIC_OAUTH_USE_PKCE" = "true";
        }
        // optionalAttrs (cfg.oidc.enable && cfg.oidc.allowedGroups != []) {
          "GF_AUTH_GENERIC_OAUTH_ALLOWED_GROUPS" = concatStringsSep " " cfg.oidc.allowedGroups;
        }
        // cfg.extraEnv;
      environmentFiles =
        [config.sops.templates."grafana-env".path]
        ++ optional cfg.oidc.enable config.sops.templates."grafana-oidc-env".path;
      volumes = [
        "${cfg.baseDir}:/var/lib/grafana"
        "${config.sops.templates."grafana-datasources".path}:/etc/grafana/provisioning/datasources/datasources.yaml:ro"
      ];
      ports = ["${toString cfg.port}:3000"];
      extraOptions =
        [
          "--network-alias=grafana"
          "--user=${toString cfg.user.uid}:${toString cfg.user.gid}"
        ]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks);
      log-driver = "journald";
    };

    systemd.services."podman-grafana" = mkMerge [
      (ociLib.mkServiceConfig {networks = cfg.networks;})
      {
        # Grafana's container user (default 472:0) needs to own the
        # bind-mount target. `install -d` is idempotent: creates the
        # directory if missing, fixes owner/perms if present, no-op if
        # already correct. Leading `+` runs as root regardless of any
        # User= setting.
        serviceConfig.ExecStartPre = [
          "+${pkgs.coreutils}/bin/install -d -o ${toString cfg.user.uid} -g ${toString cfg.user.gid} -m 0755 ${cfg.baseDir}"
        ];
      }
    ];

    networking.firewall = mkIf cfg.openFirewall {
      allowedTCPPorts = [cfg.port];
    };
  });
}
