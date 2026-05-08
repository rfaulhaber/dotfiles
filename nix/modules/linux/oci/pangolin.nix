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

  # Static traefik configuration (no secrets)
  traefikStaticConfig = pkgs.writeText "traefik_config.yml" ''
    api:
      insecure: true
      dashboard: true

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
          version: "v1.2.0"

    log:
      level: "INFO"
      format: "common"

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
        address: ":80"
      websecure:
        address: ":443"
        transport:
          respondingTimeouts:
            readTimeout: "30m"
        http:
          tls:
            certResolver: "letsencrypt"
      tcp-${toString cfg.gerbil.tcpPort}:
        address: ":${toString cfg.gerbil.tcpPort}/tcp"

    serversTransport:
      insecureSkipVerify: true
  '';

  # Dynamic traefik routing config (no secrets)
  traefikDynamicConfig = pkgs.writeText "dynamic_config.yml" ''
    http:
      middlewares:
        redirect-to-https:
          redirectScheme:
            scheme: https

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
  };

  config = mkIf cfg.enable {
    modules.linux.oci.networks.${networkName}.enable = true;

    # -- Secrets (expected in host's secrets.yaml) --
    sops.secrets = {
      "pangolin/server-secret" = {};
      "pangolin/smtp-pass" = {};
      "pangolin/admin-password" = {};
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
                window_minutes: 1
                max_requests: 100
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
            "--cap-add=NET_ADMIN"
            "--cap-add=SYS_MODULE"
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
        volumes = [
          "${traefikStaticConfig}:/etc/traefik/traefik_config.yml:ro"
          "${traefikDynamicConfig}:/etc/traefik/dynamic_config.yml:ro"
          "${cfg.baseDir}/letsencrypt:/letsencrypt:rw"
        ];
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
        (ociLib.mkServiceConfig {networks = [networkName];})
        {
          serviceConfig.ExecStartPre = [
            "${pkgs.writeShellScript "pangolin-config-init" ''
              mkdir -p ${cfg.baseDir}/pangolin
              cp -f ${config.sops.templates."pangolin-config".path} ${cfg.baseDir}/pangolin/config.yml
              chmod 0640 ${cfg.baseDir}/pangolin/config.yml
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
        serviceConfig.Restart = mkOverride 90 "always";
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
            mkdir -p ${cfg.baseDir}/letsencrypt
          ''}"
        ];
      };
    };

    networking.firewall = mkIf cfg.openFirewall {
      allowedTCPPorts = [80 443 cfg.gerbil.tcpPort];
      allowedUDPPorts = [cfg.gerbil.startPort cfg.gerbil.extraPort];
    };
  };
}
