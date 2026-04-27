{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.pihole;
  ociLib = config.modules.linux.oci.lib;

  dnsServerConf =
    pkgs.writeText "99-custom-dns.conf"
    "dhcp-option=option:dns-server,${cfg.dhcp.dnsServer}";
in {
  options.modules.linux.oci.services.pihole = {
    enable = mkEnableOption "Pi-hole DNS server";

    image = mkOption {
      description = "Pi-hole container image.";
      type = types.str;
      default = "pihole/pihole:2026.04.1";
    };

    baseDir = mkOption {
      description = "Base directory for Pi-hole data.";
      type = types.str;
      example = "/docker/pihole";
    };

    webPasswordFile = mkOption {
      description = "Path to file containing the web interface password (use sops secret path).";
      type = types.path;
      example = literalExpression "config.sops.secrets.pihole-password.path";
    };

    timezone = mkOption {
      description = "Timezone for the container.";
      type = types.str;
      default = "America/New_York";
    };

    webPort = mkOption {
      description = "Port for the web interface.";
      type = types.port;
      default = 8085;
    };

    interface = mkOption {
      description = "Network interface for DNS listening.";
      type = types.str;
      example = "end0";
    };

    dhcp = {
      enable = mkOption {
        description = "Enable DHCP server.";
        type = types.bool;
        default = false;
      };
      start = mkOption {
        description = "DHCP range start IP.";
        type = types.str;
        default = "192.168.0.3";
      };
      end = mkOption {
        description = "DHCP range end IP.";
        type = types.str;
        default = "192.168.0.253";
      };
      router = mkOption {
        description = "DHCP gateway/router IP.";
        type = types.str;
        default = "192.168.0.1";
      };
      ipv6 = mkOption {
        description = "Enable IPv6 for DHCP.";
        type = types.bool;
        default = true;
      };
      rapidCommit = mkOption {
        description = "Enable DHCP rapid commit.";
        type = types.bool;
        default = true;
      };
      dnsServer = mkOption {
        description = "DNS server IP advertised to DHCP clients. Defaults to pihole's own address when null.";
        type = types.nullOr types.str;
        default = null;
        example = "192.168.0.254";
      };
    };

    dns = {
      dnssec = mkOption {
        description = "Enable DNSSEC validation.";
        type = types.bool;
        default = true;
      };
      domainNeeded = mkOption {
        description = "Never forward plain names (without a dot or domain part).";
        type = types.bool;
        default = true;
      };
      bogusPriv = mkOption {
        description = "Never forward reverse lookups for private ranges.";
        type = types.bool;
        default = true;
      };
    };

    user = {
      uid = mkOption {
        description = "UID for pihole inside container.";
        type = types.int;
        default = config.user.uid;
      };
      gid = mkOption {
        description = "GID for pihole inside container.";
        type = types.int;
        default = 100;
      };
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci._managedPaths.${cfg.baseDir} = {};

    virtualisation.oci-containers.containers."pihole" = {
      image = cfg.image;

      # Host network mode for DNS/DHCP
      extraOptions = [
        "--network=host"
        "--cap-add=NET_ADMIN"
        "--cap-add=NET_RAW"
        "--cap-add=SYS_TIME"
      ];

      environmentFiles = [
        cfg.webPasswordFile
      ];

      environment =
        {
          "TZ" = cfg.timezone;
          "PIHOLE_UID" = toString cfg.user.uid;
          "PIHOLE_GID" = toString cfg.user.gid;
          "FTLCONF_RATE_LIMIT" = "0/0";
          "RATE_LIMIT" = "0/0";
          "FTLCONF_dns_interface" = cfg.interface;
          "FTLCONF_dns_listeningMode" = "single";
          "FTLCONF_webserver_port" = toString cfg.webPort;
          "FTLCONF_dns_dnssec" = boolToString cfg.dns.dnssec;
          "FTLCONF_dns_domainNeeded" = boolToString cfg.dns.domainNeeded;
          "FTLCONF_dns_bogusPriv" = boolToString cfg.dns.bogusPriv;
        }
        // optionalAttrs cfg.dhcp.enable {
          "FTLCONF_dhcp_active" = "true";
          "FTLCONF_dhcp_start" = cfg.dhcp.start;
          "FTLCONF_dhcp_end" = cfg.dhcp.end;
          "FTLCONF_dhcp_router" = cfg.dhcp.router;
          "FTLCONF_dhcp_ipv6" = boolToString cfg.dhcp.ipv6;
          "FTLCONF_dhcp_rapidCommit" = boolToString cfg.dhcp.rapidCommit;
        }
        // optionalAttrs (cfg.dhcp.enable && cfg.dhcp.dnsServer != null) {
          "FTLCONF_misc_etc_dnsmasq_d" = "true";
        };

      volumes =
        [
          "${cfg.baseDir}/etc-pihole:/etc/pihole"
          # "${cfg.baseDir}/etc-dnsmasq.d:/etc/dnsmasq.d"
        ]
        ++ optional (cfg.dhcp.enable && cfg.dhcp.dnsServer != null)
        "${dnsServerConf}:/etc/dnsmasq.d/99-custom-dns.conf:ro";

      log-driver = "journald";
    };

    # Pi-hole uses host networking, so it doesn't need the normal network dependencies
    # but still wants to be part of the root target for lifecycle management
    systemd.services."podman-pihole" = {
      serviceConfig = {
        Restart = mkOverride 90 "always";
      };
      partOf = ["${ociLib.rootTargetName}.target"];
      wantedBy = ["${ociLib.rootTargetName}.target"];
    };
  };
}
