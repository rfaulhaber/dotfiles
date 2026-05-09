{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.grafana;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.grafana = {
    enable = mkEnableOption "Grafana visualization server";

    image = imageLib.mkImageOptions {
      repository = "grafana/grafana";
      version = "13.0.1";
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
          # Explicit UID so dashboards can reference it deterministically
          # in the modern `{"type": "...", "uid": "..."}` datasource form.
          # Without this, Grafana would generate a random UID per
          # provisioning and any pinned reference would break.
          uid = "prometheus";
          type = "prometheus";
          access = "proxy";
          url = "http://prometheus:9090";
          isDefault = true;
        }
        {
          name = "Loki";
          uid = "loki";
          type = "loki";
          access = "proxy";
          url = "http://loki:3100";
        }
      ];
    };

    dashboardsPath = mkOption {
      description = ''
        Nix path to a directory of dashboard *.json files. The path is
        copied into the Nix store, so the bind-mount is read-only —
        edits made in the UI live in the SQLite DB until the next
        rebuild changes the store path, after which the file version
        wins. Treat the UI as a scratchpad and the JSON files as truth.
      '';
      type = types.nullOr types.path;
      default = null;
      example = literalExpression "./dashboards";
    };

    datasourceSubstitutions = mkOption {
      description = ''
        Build-time substitutions converting legacy `__inputs`-style
        datasource placeholders into modern object-form references.
        For each key K with value `{type, uid}`, the literal pattern
        `"''${K}"` (including the surrounding quotes) in dashboard
        JSONs is replaced with `{"type": "<type>", "uid": "<uid>"}`.

        The string-form reference (`"datasource": "Prometheus"`) that
        community dashboards like #13639 and #14282 emit is no longer
        reliably resolved by Grafana 13's renderer for older panel
        types (`graph`, `logs`, `singlestat`). Converting to the
        object form fixes "no data sources available" errors.

        Each `uid` here MUST match a `uid` set on a corresponding
        entry in `cfg.datasources`, otherwise the substitution
        produces a reference to a non-existent datasource.

        Set to `{}` to disable.
      '';
      type = types.attrsOf (types.submodule {
        options = {
          type = mkOption {
            type = types.str;
            description = "Datasource plugin type (e.g. prometheus, loki).";
          };
          uid = mkOption {
            type = types.str;
            description = "Datasource UID — must match a UID in cfg.datasources.";
          };
        };
      });
      default = {
        DS_PROMETHEUS = {
          type = "prometheus";
          uid = "prometheus";
        };
        DS_LOKI = {
          type = "loki";
          uid = "loki";
        };
      };
    };

    datasourceUidSubstitutions = mkOption {
      description = ''
        Bare `''${K}` placeholders that should be replaced with a literal
        UID string at build time. Use this when a community dashboard has
        already adopted Grafana's modern object-form datasource reference
        but parameterizes the UID through a template variable, e.g.

          "datasource": { "type": "prometheus", "uid": "''${ds_prometheus}" }

        Grafana 13's `provisioning` module evaluates these references
        eagerly during boot and crashes the whole module ("Datasource
        provisioning error: data source not found") if the UID isn't
        already a registered datasource — template variables haven't
        been resolved yet at that point. Substituting at build time
        sidesteps the eager-validation crash.

        Differs from `datasourceSubstitutions` in two ways: the pattern
        has no surrounding quotes (matches a UID string value, not a
        whole datasource value), and the replacement is a plain UID
        string, not a JSON object. Each value MUST match a `uid` set
        on a corresponding entry in `cfg.datasources`.

        Runs AFTER `datasourceSubstitutions`, so a key present in both
        maps would only see the bare substitution applied to whatever
        the legacy pass left untouched.

        Set to `{}` to disable.
      '';
      type = types.attrsOf types.str;
      default = {
        ds_prometheus = "prometheus";
        ds_loki = "loki";
      };
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

    # If substitutions are configured, copy the dashboards into a
    # writable derivation output and run sed across the JSON files.
    # When both substitution maps are empty, mount the source path
    # directly to skip the rebuild step.
    processedDashboardsPath =
      if cfg.dashboardsPath == null
      then null
      else if cfg.datasourceSubstitutions == {} && cfg.datasourceUidSubstitutions == {}
      then cfg.dashboardsPath
      else
        pkgs.runCommand "grafana-dashboards-processed" {} (''
            mkdir -p $out
            cp -r ${cfg.dashboardsPath}/. $out/
            chmod -R u+w $out
          ''
          + concatStringsSep "\n" (
            (mapAttrsToList (
                k: v: let
                  objectRef = builtins.toJSON {inherit (v) type uid;};
                in "${pkgs.gnused}/bin/sed -i 's|\"\${${k}}\"|${objectRef}|g' $out/*.json"
              )
              cfg.datasourceSubstitutions)
            ++ (mapAttrsToList (
                k: uid: "${pkgs.gnused}/bin/sed -i 's|\${${k}}|${uid}|g' $out/*.json"
              )
              cfg.datasourceUidSubstitutions)
          ));
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
      }
      // optionalAttrs (cfg.dashboardsPath != null) {
        "grafana-dashboards-provider" = {
          content = builtins.toJSON {
            apiVersion = 1;
            providers = [
              {
                name = "default";
                orgId = 1;
                folder = "";
                type = "file";
                disableDeletion = true;
                updateIntervalSeconds = 30;
                allowUiUpdates = true;
                options.path = "/etc/grafana/provisioning/dashboards/files";
              }
            ];
          };
          mode = "0444";
        };
      };

    virtualisation.oci-containers.containers.grafana = {
      image = imageLib.renderImage cfg.image;
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
      volumes =
        [
          "${cfg.baseDir}:/var/lib/grafana"
          "${config.sops.templates."grafana-datasources".path}:/etc/grafana/provisioning/datasources/datasources.yaml:ro"
        ]
        ++ optionals (cfg.dashboardsPath != null) [
          "${processedDashboardsPath}:/etc/grafana/provisioning/dashboards/files:ro"
          "${config.sops.templates."grafana-dashboards-provider".path}:/etc/grafana/provisioning/dashboards/provider.yaml:ro"
        ];
      ports = ["${toString cfg.port}:3000"];
      extraOptions =
        [
          "--network-alias=grafana"
          "--user=${toString cfg.user.uid}:${toString cfg.user.gid}"
        ]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ imageLib.mkImageLabels {
          module = "grafana";
          image = cfg.image;
        };
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
