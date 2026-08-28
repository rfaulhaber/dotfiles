{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.pangolin;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
  networkName = "pangolin";

  # When CrowdSec is enabled we attach its bouncer as a default middleware on
  # both HTTP entrypoints so it covers every router — including the tunneled
  # resources Pangolin generates dynamically via the http provider.
  # `@file` resolves the middleware against the file provider where it is
  # actually defined (dynamic_config.yml).
  #
  # Column accounting: the outer '' block has 4-space minimum source indent.
  # After strip, `web:` is at col 2 and `address:` at col 4. The interpolated
  # values below must place their content at the right column relative to
  # those landmarks.
  webEntryPointHttp =
    optionalString cfg.crowdsec.enable
    "\n    http:\n      middlewares:\n        - \"crowdsec-bouncer@file\"";

  # Source has `    ${...}` (4 leading spaces → post-strip col 0). To land
  # `middlewares:` at col 6 (child of websecure's http: at col 4) the value
  # itself supplies 6 leading spaces. The `- "..."` line carries its own
  # full 8-space lead since `\n` resets to col 0.
  websecureMiddlewares =
    optionalString cfg.crowdsec.enable
    "      middlewares:\n        - \"crowdsec-bouncer@file\"";

  # Inserted at end of the badger `version:` line in source (which sits at
  # col 6 after the static config's 4-space strip). We need
  # `crowdsec-bouncer-traefik-plugin:` to land at col 4 (sibling of `badger:`)
  # and its children at col 6/8.
  crowdsecPluginEntry = optionalString cfg.crowdsec.enable (
    "\n"
    + concatMapStringsSep "\n" (line: "    " + line) [
      "crowdsec-bouncer-traefik-plugin:"
      "  moduleName: \"github.com/maxlerebourg/crowdsec-bouncer-traefik-plugin\""
      "  version: \"${cfg.crowdsec.pluginVersion}\""
    ]
  );

  # Static traefik configuration (no secrets)
  traefikStaticConfig = pkgs.writeText "traefik_config.yml" ''
    # No api: block — upstream's reference config ships api.insecure (the
    # unauthenticated :8080 dashboard), but nothing in the stack consumes
    # traefik's API, and inside gerbil's netns it is reachable by every
    # container on the pangolin network.
    providers:
      http:
        endpoint: "http://pangolin:3001/api/v1/traefik-config"
        pollInterval: "5s"
      file:
        filename: "/etc/traefik/dynamic_config.yml"

    experimental:
      plugins:
        badger:
          moduleName: "github.com/fosrl/badger"
          version: "v1.4.1"${crowdsecPluginEntry}

    log:
      level: "INFO"
      format: "common"

    accessLog:
      filePath: "/var/log/traefik/access.log"
      format: "json"
      bufferingSize: 0

    certificatesResolvers:
      letsencrypt:
        acme:
          httpChallenge:
            entryPoint: web
          email: "${cfg.adminEmail}"
          storage: "/letsencrypt/acme.json"
          caServer: "https://acme-v02.api.letsencrypt.org/directory"

    entryPoints:
      web:
        address: ":80"${webEntryPointHttp}
      websecure:
        address: ":443"
        transport:
          respondingTimeouts:
            readTimeout: "30m"
        http:
    ${websecureMiddlewares}
          tls:
            certResolver: "letsencrypt"
      tcp-${toString cfg.gerbil.tcpPort}:
        address: ":${toString cfg.gerbil.tcpPort}/tcp"

    serversTransport:
      insecureSkipVerify: true
  '';

  # Built as an indented list of YAML lines so nix '' strip-indent doesn't
  # eat our nesting. Each list entry is the YAML body; we prefix 4 spaces
  # because the dynamic_config outer '' has 4-space min-strip, and we want
  # `crowdsec-bouncer:` to land at col 4 (sibling of `redirect-to-https:`).
  crowdsecMiddlewareInline = optionalString cfg.crowdsec.enable (
    "\n"
    + concatMapStringsSep "\n" (line: "    " + line) [
      "crowdsec-bouncer:"
      "  plugin:"
      "    crowdsec-bouncer-traefik-plugin:"
      "      enabled: true"
      "      logLevel: INFO"
      "      crowdsecMode: ${cfg.crowdsec.mode}"
      "      updateIntervalSeconds: ${toString cfg.crowdsec.updateIntervalSeconds}"
      "      defaultDecisionSeconds: ${toString cfg.crowdsec.defaultDecisionSeconds}"
      "      crowdsecLapiScheme: http"
      "      crowdsecLapiHost: \"${cfg.crowdsec.lapiHost}\""
      "      crowdsecLapiKeyFile: \"${cfg.crowdsec.bouncerKeyContainerPath}\""
      "      forwardedHeadersTrustedIPs: []"
      "      clientTrustedIPs: []"
    ]
  );

  # Dynamic traefik routing config (no secrets). The CrowdSec bouncer key
  # itself is mounted as a file (see the traefik volumes block) and read by
  # the plugin via crowdsecLapiKeyFile, so the API key never appears in this
  # rendered file even when crowdsec.enable = true.
  traefikDynamicConfig = pkgs.writeText "dynamic_config.yml" ''
    http:
      middlewares:
        redirect-to-https:
          redirectScheme:
            scheme: https${crowdsecMiddlewareInline}

      routers:
        main-app-router-redirect:
          rule: "Host(`${cfg.dashboardDomain}`)"
          service: next-service
          entryPoints:
            - web
          middlewares:
            - redirect-to-https

        next-router:
          rule: "Host(`${cfg.dashboardDomain}`) && !PathPrefix(`/api/v1`)"
          service: next-service
          entryPoints:
            - websecure
          tls:
            certResolver: letsencrypt

        api-router:
          rule: "Host(`${cfg.dashboardDomain}`) && PathPrefix(`/api/v1`)"
          service: api-service
          entryPoints:
            - websecure
          tls:
            certResolver: letsencrypt

        ws-router:
          rule: "Host(`${cfg.dashboardDomain}`)"
          service: api-service
          entryPoints:
            - websecure
          tls:
            certResolver: letsencrypt

      services:
        next-service:
          loadBalancer:
            servers:
              - url: "http://pangolin:3002"

        api-service:
          loadBalancer:
            servers:
              - url: "http://pangolin:3000"
  '';
in {
  options.modules.linux.oci.services.pangolin = {
    enable = mkEnableOption "Pangolin reverse proxy stack (pangolin + gerbil + traefik)";

    domain = mkOption {
      description = "Base domain for Pangolin services.";
      type = types.str;
      example = "3679.space";
    };

    dashboardDomain = mkOption {
      description = "FQDN for the Pangolin dashboard.";
      type = types.str;
      example = "pangolin.3679.space";
    };

    bindAddress = mkOption {
      description = "IP address to bind published ports to.";
      type = types.str;
      example = "66.63.168.244";
    };

    baseDir = mkOption {
      description = "Base directory for all Pangolin stack state (pangolin, gerbil, traefik, letsencrypt).";
      type = types.str;
      example = "/data/apps/pangolin";
    };

    adminEmail = mkOption {
      description = "Admin email address (used for ACME and initial admin account).";
      type = types.str;
    };

    email = {
      smtpHost = mkOption {
        type = types.str;
        default = "smtp.fastmail.com";
      };
      smtpPort = mkOption {
        type = types.port;
        default = 465;
      };
      smtpUser = mkOption {
        type = types.str;
      };
      noReply = mkOption {
        type = types.str;
        example = "no-reply@3679.space";
      };
    };

    gerbil = {
      startPort = mkOption {
        description = "WireGuard starting port for gerbil.";
        type = types.port;
        default = 51820;
      };
      extraPort = mkOption {
        description = "Additional WireGuard UDP port.";
        type = types.port;
        default = 21820;
      };
      tcpPort = mkOption {
        description = "TCP proxy port for raw resources.";
        type = types.port;
        default = 3402;
      };
      subnetGroup = mkOption {
        description = "Subnet CIDR for gerbil tunnel allocation.";
        type = types.str;
        default = "100.89.137.0/20";
      };
    };

    images = {
      pangolin = imageLib.mkImageOptions {
        repository = "fosrl/pangolin";
        version = "latest";
      };
      gerbil = imageLib.mkImageOptions {
        repository = "fosrl/gerbil";
        version = "latest";
      };
      traefik = imageLib.mkImageOptions {
        repository = "traefik";
        version = "v3.4.0";
      };
    };

    openFirewall = mkOption {
      description = "Whether to open firewall ports for the Pangolin stack.";
      type = types.bool;
      default = false;
    };

    rateLimit = {
      windowMinutes = mkOption {
        description = "Rate-limit window for Pangolin's external API (port 3000).";
        type = types.int;
        default = 1;
      };
      maxRequests = mkOption {
        description = ''
          Max requests per window across all clients for Pangolin's external
          API. This caps dashboard + login + admin traffic only. Tunneled
          resources (e.g. Jellyfin) bypass it because they hit Pangolin's
          internal port (3001) via badger, not the external 3000.
        '';
        type = types.int;
        default = 30;
      };
    };

    traefikLogPath = mkOption {
      description = ''
        Host path Traefik writes its JSON access log to. Reference from
        CrowdSec's acquisitions config when wiring intrusion detection.
      '';
      type = types.str;
      default = "${cfg.baseDir}/traefik/access.log";
      defaultText = literalExpression ''"''${cfg.baseDir}/traefik/access.log"'';
    };

    crowdsec = {
      enable = mkEnableOption ''
        CrowdSec bouncer integration in Traefik. Loads the bouncer plugin and
        attaches it as a global middleware on web/websecure entrypoints. The
        CrowdSec engine itself is configured separately via
        modules.linux.oci.services.crowdsec
      '';

      pluginVersion = mkOption {
        description = "Version tag of crowdsec-bouncer-traefik-plugin to load.";
        type = types.str;
        default = "v1.4.2";
      };

      mode = mkOption {
        description = ''
          Bouncer decision-fetch mode. `stream` polls LAPI on a fixed interval
          and caches decisions in memory (no per-request latency). `live`
          queries LAPI synchronously on every request (more accurate, slight
          per-request cost).
        '';
        type = types.enum ["stream" "live" "alone" "appsec"];
        default = "stream";
      };

      updateIntervalSeconds = mkOption {
        description = "How often (seconds) to refresh the decision cache in stream mode.";
        type = types.int;
        default = 60;
      };

      defaultDecisionSeconds = mkOption {
        description = "Cache TTL for individual decisions in live mode.";
        type = types.int;
        default = 60;
      };

      lapiHost = mkOption {
        description = ''
          host:port of the CrowdSec LAPI as resolvable from inside the Traefik
          container. Default assumes the CrowdSec engine runs as
          --network-alias=crowdsec on the same podman network as gerbil
          (Traefik joins gerbil's netns).
        '';
        type = types.str;
        default = "crowdsec:8080";
      };

      bouncerKeyFile = mkOption {
        description = ''
          Host path to the sops-rendered file containing the bouncer API key.
          Bind-mounted into the Traefik container at bouncerKeyContainerPath
          and read by the plugin via crowdsecLapiKeyFile (so the key never
          appears in any rendered nix file).
        '';
        type = types.str;
        default = config.sops.secrets."crowdsec/bouncer-api-key".path or "";
        defaultText = literalExpression ''config.sops.secrets."crowdsec/bouncer-api-key".path'';
      };

      bouncerKeyContainerPath = mkOption {
        description = "Where to mount bouncerKeyFile inside the Traefik container.";
        type = types.str;
        default = "/secrets/crowdsec-bouncer-key";
      };
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci.networks.${networkName}.enable = true;

    # Gerbil creates its wg interface purely via netlink and hard-fails if the
    # wireguard module is absent — the image has no kmod and the container
    # deliberately lacks SYS_MODULE, so the host must guarantee the module.
    boot.kernelModules = ["wireguard"];

    # -- Secrets (expected in host's secrets.yaml) --
    sops.secrets =
      {
        "pangolin/server-secret" = {};
        "pangolin/smtp-pass" = {};
        "pangolin/admin-password" = {};
      }
      // optionalAttrs cfg.crowdsec.enable {
        "crowdsec/bouncer-api-key" = {};
      };

    # -- Pangolin config.yml template (has embedded secrets) --
    sops.templates."pangolin-config" = {
      content = ''
        app:
            dashboard_url: "https://${cfg.dashboardDomain}"
            log_level: info
            save_logs: false
        domains:
            domain1:
                base_domain: "${cfg.domain}"
                cert_resolver: letsencrypt
                prefer_wildcard_cert: false
        server:
            external_port: 3000
            internal_port: 3001
            next_port: 3002
            internal_hostname: pangolin
            session_cookie_name: p_session_token
            resource_access_token_param: p_token
            resource_access_token_headers:
                id: P-Access-Token-Id
                token: P-Access-Token
            resource_session_request_param: p_session_request
            secret: "${config.sops.placeholder."pangolin/server-secret"}"
        traefik:
            cert_resolver: letsencrypt
            http_entrypoint: web
            https_entrypoint: websecure
        gerbil:
            start_port: ${toString cfg.gerbil.startPort}
            base_endpoint: "${cfg.dashboardDomain}"
            use_subdomain: false
            block_size: 24
            site_block_size: 30
            subnet_group: "${cfg.gerbil.subnetGroup}"
        rate_limits:
            global:
                window_minutes: ${toString cfg.rateLimit.windowMinutes}
                max_requests: ${toString cfg.rateLimit.maxRequests}
        email:
            smtp_host: "${cfg.email.smtpHost}"
            smtp_port: ${toString cfg.email.smtpPort}
            smtp_user: "${cfg.email.smtpUser}"
            smtp_pass: "${config.sops.placeholder."pangolin/smtp-pass"}"
            no_reply: "${cfg.email.noReply}"
        users:
            server_admin:
                email: "${cfg.adminEmail}"
                password: "${config.sops.placeholder."pangolin/admin-password"}"
        flags:
            require_email_verification: true
            disable_signup_without_invite: true
            disable_user_create_org: true
            allow_raw_resources: true
            allow_base_domain_resources: true
      '';
    };

    # -- Containers --
    virtualisation.oci-containers.containers = {
      "pangolin" = {
        image = imageLib.renderImage cfg.images.pangolin;
        volumes = [
          "${cfg.baseDir}/pangolin:/app/config:rw"
          # Pangolin ≥1.22.0 syncs cert status from traefik's acme.json
          # (default path /app/config/letsencrypt/acme.json, relative to
          # WORKDIR). Read-only: the file holds every cert's private key and
          # the sync only reads; all its writes go to pangolin's own DB.
          "${cfg.baseDir}/letsencrypt:/app/config/letsencrypt:ro"
        ];
        extraOptions =
          [
            "--network-alias=pangolin"
            "--network=${ociLib.networkName networkName}"
          ]
          ++ imageLib.mkImageLabels {
            module = "pangolin.pangolin";
            image = cfg.images.pangolin;
          };
        log-driver = "journald";
      };

      "gerbil" = {
        image = imageLib.renderImage cfg.images.gerbil;
        dependsOn = ["pangolin"];
        cmd = [
          "--reachableAt=http://gerbil:3003"
          "--generateAndSaveKeyTo=/var/config/key"
          "--remoteConfig=http://pangolin:3001/api/v1/"
        ];
        volumes = [
          "${cfg.baseDir}/gerbil:/var/config:rw"
        ];
        ports = [
          "${cfg.bindAddress}:${toString cfg.gerbil.startPort}:${toString cfg.gerbil.startPort}/udp"
          "${cfg.bindAddress}:${toString cfg.gerbil.extraPort}:${toString cfg.gerbil.extraPort}/udp"
          "${cfg.bindAddress}:443:443"
          "${cfg.bindAddress}:80:80"
          "${cfg.bindAddress}:${toString cfg.gerbil.tcpPort}:${toString cfg.gerbil.tcpPort}"
        ];
        extraOptions =
          [
            "--network-alias=gerbil"
            "--network=${ociLib.networkName networkName}"
            # Netlink in gerbil's own netns is all interface creation needs;
            # the wireguard module is preloaded host-side (boot.kernelModules
            # above). SYS_MODULE would let a compromised public-ingress
            # container load host kernel modules.
            "--cap-add=NET_ADMIN"
          ]
          ++ imageLib.mkImageLabels {
            module = "pangolin.gerbil";
            image = cfg.images.gerbil;
          };
        log-driver = "journald";
      };

      # Traefik shares gerbil's network namespace (ports published on gerbil)
      "traefik" = {
        image = imageLib.renderImage cfg.images.traefik;
        dependsOn = ["pangolin"];
        cmd = ["--configFile=/etc/traefik/traefik_config.yml"];
        volumes =
          [
            "${traefikStaticConfig}:/etc/traefik/traefik_config.yml:ro"
            "${traefikDynamicConfig}:/etc/traefik/dynamic_config.yml:ro"
            "${cfg.baseDir}/letsencrypt:/letsencrypt:rw"
            "${cfg.baseDir}/traefik:/var/log/traefik:rw"
          ]
          ++ optional cfg.crowdsec.enable
          "${cfg.crowdsec.bouncerKeyFile}:${cfg.crowdsec.bouncerKeyContainerPath}:ro";
        extraOptions =
          [
            "--network=container:gerbil"
          ]
          ++ imageLib.mkImageLabels {
            module = "pangolin.traefik";
            image = cfg.images.traefik;
          };
        log-driver = "journald";
      };
    };

    # -- Systemd service configuration --
    systemd.services = {
      "podman-pangolin" = mkMerge [
        (ociLib.mkServiceConfig {
          networks = [networkName];
          sopsTemplates = ["pangolin-config"];
        })
        {
          serviceConfig.ExecStartPre = [
            "${pkgs.writeShellScript "pangolin-config-init" ''
              mkdir -p ${cfg.baseDir}/pangolin
              cp -f ${config.sops.templates."pangolin-config".path} ${cfg.baseDir}/pangolin/config.yml
              # config.yml embeds the server secret, SMTP password and admin
              # password — keep it root-only (0640 + group `users` left it
              # readable by any second account on the box).
              chown root:root ${cfg.baseDir}/pangolin/config.yml
              chmod 0600 ${cfg.baseDir}/pangolin/config.yml
            ''}"
          ];
        }
      ];

      "podman-gerbil" = mkMerge [
        (ociLib.mkServiceConfig {networks = [networkName];})
        {
          after = ["podman-pangolin.service"];
          requires = ["podman-pangolin.service"];
          serviceConfig.ExecStartPre = [
            "${pkgs.writeShellScript "gerbil-dir-init" ''
              mkdir -p ${cfg.baseDir}/gerbil
            ''}"
          ];
        }
      ];

      # Traefik uses container:gerbil networking — no podman network deps
      "podman-traefik" = {
        # See mkServiceConfig in default.nix: keep retrying through transient
        # startup failures rather than latching to the start-limit.
        startLimitIntervalSec = mkOverride 90 0;
        serviceConfig.Restart = mkOverride 90 "always";
        serviceConfig.RestartSec = mkOverride 90 10;
        after = [
          "podman-pangolin.service"
          "podman-gerbil.service"
        ];
        requires = [
          "podman-gerbil.service"
        ];
        partOf = ["${ociLib.rootTargetName}.target"];
        wantedBy = ["${ociLib.rootTargetName}.target"];
        serviceConfig.ExecStartPre = [
          "${pkgs.writeShellScript "traefik-dir-init" ''
            mkdir -p ${cfg.baseDir}/letsencrypt ${cfg.baseDir}/traefik
            # Pre-create the access log so CrowdSec's tailer finds the
            # configured filename immediately (its acquisition mounts the
            # directory, so this is a convenience, not a requirement).
            touch ${cfg.baseDir}/traefik/access.log
          ''}"
        ];
      };
    };

    # Traefik keeps the access log's fd open inside the container, so rotate
    # with copytruncate rather than signaling a reopen across the netns
    # boundary. CrowdSec tails by filename and follows the truncation.
    services.logrotate.settings."pangolin-traefik-access" = {
      files = cfg.traefikLogPath;
      frequency = "weekly";
      rotate = 8;
      compress = true;
      copytruncate = true;
      missingok = true;
      notifempty = true;
    };

    # The Pangolin DB (and its timestamped backups) hold bcrypt password
    # hashes and live session tokens. Keep both directories root-only so a
    # second host account or a container that bind-mounts a parent path can't
    # read them, and age-prune the backups so old credential snapshots don't
    # accumulate indefinitely. `d ... 30d` enforces the directory mode on
    # every tmpfiles run and removes contents older than 30 days on the daily
    # clean; `z` reasserts the live DB file mode.
    systemd.tmpfiles.rules = [
      "d ${cfg.baseDir}/pangolin/db 0700 root root -"
      "z ${cfg.baseDir}/pangolin/db/db.sqlite 0600 root root -"
      "d ${cfg.baseDir}/pangolin/db/backups 0700 root root 30d"
    ];

    networking.firewall = mkIf cfg.openFirewall {
      allowedTCPPorts = [80 443 cfg.gerbil.tcpPort];
      allowedUDPPorts = [cfg.gerbil.startPort cfg.gerbil.extraPort];
    };
  };
}
