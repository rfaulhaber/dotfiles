{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.netbird;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
  networkName = "netbird";

  # Management config generated from Nix attrset → JSON via sops template
  managementConfig = {
    Stuns = [
      {
        Proto = "udp";
        URI = "stun:${cfg.domain}:${toString cfg.ports.stun}";
        Username = "";
        Password = null;
      }
    ];
    TURNConfig = {
      Turns = [
        {
          Proto = "udp";
          URI = "turn:${cfg.domain}:${toString cfg.ports.stun}";
          Username = "self";
          Password = config.sops.placeholder."netbird/turn-password";
        }
      ];
      CredentialsTTL = "12h";
      # Only consumed when TimeBasedCredentials = true (HMAC secret for
      # ephemeral TURN credentials). Coturn here validates the static
      # user=self:<turn-password> lt-cred-mech pair instead, so this value
      # is never used for auth.
      Secret = "unused";
      TimeBasedCredentials = false;
    };
    Relay = {
      Addresses = ["rel://${cfg.domain}:${toString cfg.ports.relay}"];
      CredentialsTTL = "24h";
      Secret = config.sops.placeholder."netbird/relay-secret";
    };
    Signal = {
      Proto = "http";
      URI = "${cfg.domain}:${toString cfg.ports.signal}";
      Username = "";
      Password = null;
    };
    ReverseProxy = {
      TrustedHTTPProxies = [];
      TrustedHTTPProxiesCount = 0;
      # Nothing proxies for the management API — its port publishes directly
      # on bindAddress — so no client should be able to spoof its address via
      # forwarded headers. Upstream treats an EMPTY list as 0.0.0.0/0 (trust
      # everyone), so loopback is the tightest expressible value.
      TrustedPeers = ["127.0.0.1/32"];
    };
    Datadir = "";
    # A real key must be rendered here. Left empty, management generates one
    # and persists it back into management.json — which the ExecStartPre
    # template copy re-blanks on the next start, minting a fresh key every
    # restart and orphaning the AES-GCM-encrypted store fields (user
    # email/name, invites) under the old one. 32-byte base64
    # (openssl rand -base64 32).
    DataStoreEncryptionKey = config.sops.placeholder."netbird/data-store-encryption-key";
    StoreConfig = {
      Engine = "sqlite";
    };
    HttpConfig = {
      Address = "0.0.0.0:${toString cfg.ports.management}";
      AuthIssuer = "https://${cfg.authDomain}";
      AuthAudience = config.sops.placeholder."netbird/auth-audience";
      AuthKeysLocation = "https://${cfg.authDomain}/.well-known/jwks.json";
      AuthUserIDClaim = "";
      CertFile = "/etc/letsencrypt/live/${cfg.domain}/fullchain.pem";
      CertKey = "/etc/letsencrypt/live/${cfg.domain}/privkey.pem";
      IdpSignKeyRefreshEnabled = false;
      OIDCConfigEndpoint = "https://${cfg.authDomain}/.well-known/openid-configuration";
    };
    IdpManagerConfig = {
      ManagerType = "";
      ClientConfig = {
        Issuer = "https://${cfg.authDomain}";
        TokenEndpoint = "https://${cfg.authDomain}/api/oidc/token";
        ClientID = "";
        ClientSecret = "";
        GrantType = "client_credentials";
      };
      ExtraConfig = {};
      Auth0ClientCredentials = null;
      AzureClientCredentials = null;
      KeycloakClientCredentials = null;
      ZitadelClientCredentials = null;
    };
    DeviceAuthorizationFlow = {
      Provider = "hosted";
      ProviderConfig = {
        Audience = config.sops.placeholder."netbird/auth-audience";
        AuthorizationEndpoint = "";
        Domain = "";
        ClientID = config.sops.placeholder."netbird/auth-client-id";
        ClientSecret = "";
        TokenEndpoint = "https://${cfg.authDomain}/api/oidc/token";
        DeviceAuthEndpoint = "https://${cfg.authDomain}/api/oidc/device/authorize";
        Scope = "openid";
        UseIDToken = false;
        RedirectURLs = null;
      };
    };
    PKCEAuthorizationFlow = {
      ProviderConfig = {
        Audience = config.sops.placeholder."netbird/auth-audience";
        ClientID = config.sops.placeholder."netbird/auth-client-id";
        ClientSecret = "";
        Domain = "";
        AuthorizationEndpoint = "https://${cfg.authDomain}/authorize";
        TokenEndpoint = "https://${cfg.authDomain}/api/oidc/token";
        Scope = "";
        RedirectURLs = ["http://localhost:53000"];
        UseIDToken = false;
        DisablePromptLogin = false;
        LoginFlag = 1;
      };
    };
  };
in {
  options.modules.linux.oci.services.netbird = {
    enable = mkEnableOption "NetBird self-hosted stack (dashboard, signal, relay, management, coturn)";

    domain = mkOption {
      description = "FQDN for NetBird services.";
      type = types.str;
      example = "netbird.3679.space";
    };

    authDomain = mkOption {
      description = "FQDN of the OIDC provider (e.g. Pocket-ID).";
      type = types.str;
      example = "auth.3679.space";
    };

    bindAddress = mkOption {
      description = "IP address to bind published ports to.";
      type = types.str;
      example = "66.63.168.153";
    };

    baseDir = mkOption {
      description = "Base directory for NetBird persistent state that needs host-path storage.";
      type = types.str;
      example = "/data/apps/netbird";
    };

    acmeEmail = mkOption {
      description = "Email for Let's Encrypt certificates (dashboard).";
      type = types.str;
    };

    ports = {
      dashboardHttp = mkOption {
        type = types.port;
        default = 80;
      };
      dashboardHttps = mkOption {
        type = types.port;
        default = 443;
      };
      signal = mkOption {
        type = types.port;
        default = 10000;
      };
      relay = mkOption {
        type = types.port;
        default = 33080;
      };
      management = mkOption {
        description = "External port for the management API.";
        type = types.port;
        default = 33073;
      };
      stun = mkOption {
        description = "STUN/TURN listener port (coturn, host network).";
        type = types.port;
        default = 3478;
      };
      stunTls = mkOption {
        description = "STUN/TURN TLS listener port (coturn, host network).";
        type = types.port;
        default = 5349;
      };
    };

    coturn = {
      realm = mkOption {
        type = types.str;
        default = "wiretrustee.com";
      };
      minPort = mkOption {
        description = "Lower bound of coturn's UDP relay allocation range (also opened in the firewall).";
        type = types.port;
        default = 49152;
      };
      maxPort = mkOption {
        description = ''
          Upper bound of the relay allocation range. Each concurrent TURN
          allocation consumes one port, and TURN is only NetBird's fallback
          when a direct WireGuard path can't be established — 200 ports is
          generous for a small fleet. Upstream's compose opens the whole
          ephemeral range (49152-65535) on the public IP; don't restore
          that without a concrete allocation-exhaustion signal.
        '';
        type = types.int;
        default = 49351;
      };
    };

    images = {
      dashboard = imageLib.mkImageOptions {
        repository = "netbirdio/dashboard";
        version = "latest";
      };
      signal = imageLib.mkImageOptions {
        repository = "netbirdio/signal";
        version = "latest";
      };
      relay = imageLib.mkImageOptions {
        repository = "netbirdio/relay";
        version = "latest";
      };
      management = imageLib.mkImageOptions {
        repository = "netbirdio/management";
        version = "latest";
      };
      coturn = imageLib.mkImageOptions {
        repository = "coturn/coturn";
        version = "latest";
      };
    };

    openFirewall = mkOption {
      description = "Whether to open firewall ports for NetBird.";
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci.networks.${networkName}.enable = true;

    # Named volumes for persistent state
    modules.linux.oci.volumes = {
      "netbird-mgmt".enable = true;
      "netbird-signal".enable = true;
      "netbird-letsencrypt".enable = true;
    };

    # -- Secrets (expected in host's secrets.yaml) --
    sops.secrets = {
      "netbird/auth-audience" = {};
      "netbird/auth-client-id" = {};
      "netbird/auth-client-secret" = {};
      "netbird/nb-auth-secret" = {};
      "netbird/turn-password" = {};
      "netbird/relay-secret" = {};
      "netbird/data-store-encryption-key" = {};
    };

    # -- management.json template --
    sops.templates."netbird-management".content = builtins.toJSON managementConfig;

    # -- turnserver.conf template (one secret: user password) --
    sops.templates."netbird-turnserver".content = ''
      listening-port=${toString cfg.ports.stun}
      tls-listening-port=${toString cfg.ports.stunTls}
      min-port=${toString cfg.coturn.minPort}
      max-port=${toString cfg.coturn.maxPort}
      fingerprint
      lt-cred-mech
      user=self:${config.sops.placeholder."netbird/turn-password"}
      realm=${cfg.coturn.realm}
      cert=/etc/coturn/certs/cert.pem
      pkey=/etc/coturn/private/privkey.pem
      log-file=stdout
      no-software-attribute
      no-cli
      pidfile="/var/tmp/turnserver.pid"
    '';

    # -- Dashboard auth env template (secrets as KEY=VALUE) --
    sops.templates."netbird-dashboard-env".content = ''
      AUTH_AUDIENCE=${config.sops.placeholder."netbird/auth-audience"}
      AUTH_CLIENT_ID=${config.sops.placeholder."netbird/auth-client-id"}
      AUTH_CLIENT_SECRET=${config.sops.placeholder."netbird/auth-client-secret"}
    '';

    # -- Relay auth secret env template --
    sops.templates."netbird-relay-env".content = ''
      NB_AUTH_SECRET=${config.sops.placeholder."netbird/nb-auth-secret"}
    '';

    # -- Containers --
    virtualisation.oci-containers.containers = {
      "netbird-dashboard" = {
        image = imageLib.renderImage cfg.images.dashboard;
        environment = {
          "NETBIRD_MGMT_API_ENDPOINT" = "https://${cfg.domain}:${toString cfg.ports.management}";
          "NETBIRD_MGMT_GRPC_API_ENDPOINT" = "https://${cfg.domain}:${toString cfg.ports.management}";
          "AUTH_AUTHORITY" = "https://${cfg.authDomain}";
          "USE_AUTH0" = "false";
          "AUTH_SUPPORTED_SCOPES" = "openid profile email groups";
          "AUTH_REDIRECT_URI" = "/auth";
          "AUTH_SILENT_REDIRECT_URI" = "/silent-auth";
          "NETBIRD_TOKEN_SOURCE" = "accessToken";
          "NGINX_SSL_PORT" = toString cfg.ports.dashboardHttps;
          "LETSENCRYPT_DOMAIN" = cfg.domain;
          "LETSENCRYPT_EMAIL" = cfg.acmeEmail;
        };
        environmentFiles = [
          config.sops.templates."netbird-dashboard-env".path
        ];
        ports = [
          "${cfg.bindAddress}:${toString cfg.ports.dashboardHttp}:80"
          "${cfg.bindAddress}:${toString cfg.ports.dashboardHttps}:443"
        ];
        volumes = [
          "${ociLib.volumeName "netbird-letsencrypt"}:/etc/letsencrypt/"
        ];
        extraOptions =
          [
            "--network-alias=netbird-dashboard"
            "--network=${ociLib.networkName networkName}"
          ]
          ++ imageLib.mkImageLabels {
            module = "netbird.dashboard";
            image = cfg.images.dashboard;
          };
        log-driver = "journald";
      };

      "netbird-signal" = {
        image = imageLib.renderImage cfg.images.signal;
        volumes = [
          "${ociLib.volumeName "netbird-signal"}:/var/lib/netbird"
        ];
        ports = [
          "${cfg.bindAddress}:${toString cfg.ports.signal}:80"
        ];
        extraOptions =
          [
            "--network-alias=netbird-signal"
            "--network=${ociLib.networkName networkName}"
          ]
          ++ imageLib.mkImageLabels {
            module = "netbird.signal";
            image = cfg.images.signal;
          };
        log-driver = "journald";
      };

      "netbird-relay" = {
        image = imageLib.renderImage cfg.images.relay;
        environment = {
          "NB_LOG_LEVEL" = "info";
          "NB_LISTEN_ADDRESS" = ":${toString cfg.ports.relay}";
          "NB_EXPOSED_ADDRESS" = "rel://${cfg.domain}:${toString cfg.ports.relay}";
        };
        environmentFiles = [
          config.sops.templates."netbird-relay-env".path
        ];
        ports = [
          "${cfg.bindAddress}:${toString cfg.ports.relay}:${toString cfg.ports.relay}"
        ];
        extraOptions =
          [
            "--network-alias=netbird-relay"
            "--network=${ociLib.networkName networkName}"
          ]
          ++ imageLib.mkImageLabels {
            module = "netbird.relay";
            image = cfg.images.relay;
          };
        log-driver = "journald";
      };

      "netbird-mgmt" = {
        image = imageLib.renderImage cfg.images.management;
        dependsOn = ["netbird-dashboard"];
        cmd = [
          "--port"
          "443"
          "--log-file"
          "console"
          "--log-level"
          "info"
          "--disable-anonymous-metrics=false"
          "--single-account-mode-domain=${cfg.domain}"
          "--dns-domain=netbird.selfhosted"
        ];
        volumes = [
          "${ociLib.volumeName "netbird-mgmt"}:/var/lib/netbird"
          "${ociLib.volumeName "netbird-letsencrypt"}:/etc/letsencrypt:ro"
          "${cfg.baseDir}/management.json:/etc/netbird/management.json:rw"
        ];
        ports = [
          "${cfg.bindAddress}:${toString cfg.ports.management}:443"
        ];
        extraOptions =
          [
            "--network-alias=netbird-mgmt"
            "--network=${ociLib.networkName networkName}"
          ]
          ++ imageLib.mkImageLabels {
            module = "netbird.management";
            image = cfg.images.management;
          };
        log-driver = "journald";
      };

      # Coturn runs on the host network (needs direct UDP access)
      "netbird-coturn" = {
        image = imageLib.renderImage cfg.images.coturn;
        cmd = ["-c" "/etc/turnserver.conf"];
        volumes = [
          "${config.sops.templates."netbird-turnserver".path}:/etc/turnserver.conf:ro"
        ];
        extraOptions =
          [
            "--network=host"
          ]
          ++ imageLib.mkImageLabels {
            module = "netbird.coturn";
            image = cfg.images.coturn;
          };
        log-driver = "journald";
      };
    };

    # -- Systemd service configuration --
    systemd.services = {
      "podman-netbird-dashboard" = ociLib.mkServiceConfig {
        networks = [networkName];
        volumes = ["netbird-letsencrypt"];
        sopsTemplates = ["netbird-dashboard-env"];
      };

      "podman-netbird-signal" = ociLib.mkServiceConfig {
        networks = [networkName];
        volumes = ["netbird-signal"];
      };

      "podman-netbird-relay" = ociLib.mkServiceConfig {
        networks = [networkName];
        sopsTemplates = ["netbird-relay-env"];
      };

      "podman-netbird-mgmt" = mkMerge [
        (ociLib.mkServiceConfig {
          networks = [networkName];
          volumes = ["netbird-mgmt" "netbird-letsencrypt"];
          sopsTemplates = ["netbird-management"];
        })
        {
          after = ["podman-netbird-dashboard.service"];
          requires = ["podman-netbird-dashboard.service"];
          serviceConfig.ExecStartPre = [
            "${pkgs.writeShellScript "netbird-mgmt-config-init" ''
              mkdir -p ${cfg.baseDir}
              cp -f ${config.sops.templates."netbird-management".path} ${cfg.baseDir}/management.json
              chmod 0640 ${cfg.baseDir}/management.json
            ''}"
            # netbird-management exits 1 at boot if it can't fetch the IdP's
            # OIDC discovery doc. During a co-deploy the reverse proxy +
            # pocket-id chain returns 404 for a few seconds while traefik loads
            # its routes, which burns netbird-mgmt's restart budget
            # (start-limit-hit) and fails the switch. systemd ordering (after=)
            # can't gate on this: the proxy unit is `active` before it actually
            # routes. So probe the real URL netbird uses until it 200s.
            "${pkgs.writeShellScript "netbird-mgmt-wait-oidc" ''
              url="https://${cfg.authDomain}/.well-known/openid-configuration"
              for _ in $(seq 1 60); do
                code=$(${pkgs.curl}/bin/curl -s -o /dev/null -w '%{http_code}' --max-time 5 "$url" || true)
                if [ "$code" = "200" ]; then exit 0; fi
                sleep 2
              done
              echo "netbird-mgmt: OIDC discovery $url not ready after ~120s" >&2
              exit 1
            ''}"
          ];
        }
      ];

      # Coturn uses host networking — no podman network deps
      "podman-netbird-coturn" = {
        # See mkServiceConfig in default.nix: keep retrying through transient
        # startup failures rather than latching to the start-limit.
        startLimitIntervalSec = mkOverride 90 0;
        serviceConfig.Restart = mkOverride 90 "always";
        serviceConfig.RestartSec = mkOverride 90 10;
        partOf = ["${ociLib.rootTargetName}.target"];
        wantedBy = ["${ociLib.rootTargetName}.target"];
      };
    };

    networking.firewall = mkIf cfg.openFirewall {
      allowedTCPPorts = [
        cfg.ports.dashboardHttp
        cfg.ports.dashboardHttps
        cfg.ports.signal
        cfg.ports.relay
        cfg.ports.management
      ];
      allowedUDPPorts = [
        cfg.ports.stun
        cfg.ports.stunTls
      ];
      allowedUDPPortRanges = [
        {
          from = cfg.coturn.minPort;
          to = cfg.coturn.maxPort;
        }
      ];
    };
  };
}
