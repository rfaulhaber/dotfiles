{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.pihole;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};

  # Extra raw dnsmasq directives rendered into /etc/dnsmasq.d. Pi-hole (FTL)
  # loads these when misc.etc_dnsmasq_d is enabled.
  dnsmasqExtraLines =
    optional (cfg.dhcp.enable && cfg.dhcp.dnsServer != null)
    "dhcp-option=option:dns-server,${cfg.dhcp.dnsServer}"
    ++ optional (cfg.dhcp.enable && cfg.dhcp.ipv6 && cfg.dhcp.dnsServerV6 != null)
    # Pin the IPv6 DNS server advertised via the RDNSS option (in RAs) and
    # DHCPv6. Without this, dnsmasq auto-selects one of the host's own
    # addresses, which can change across restarts and — critically — does NOT
    # survive keepalived failover. Point it at the floating VIP so v6 DNS
    # follows whichever node currently holds it, mirroring the v4 dnsServer.
    "dhcp-option=option6:dns-server,[${cfg.dhcp.dnsServerV6}]"
    ++ optional (cfg.dhcp.enable && cfg.dhcp.ipv6 && !cfg.dhcp.ipv6Router)
    # Keep sending Router Advertisements — so the RDNSS option still tells
    # IPv6 clients to use Pi-hole for DNS (RFC 8106 decouples RDNSS lifetime
    # from the router lifetime) — but set the router lifetime to 0 so clients
    # do NOT install this host as a default IPv6 gateway. Without this a
    # DNS-only appliance with dhcp.ipv6 becomes a black-hole default route.
    # ra-param fields: <interface>,<ra-interval>,<router-lifetime>
    # (an interval of 0 selects dnsmasq's default interval).
    "ra-param=${cfg.interface},0,0"
    ++ map (record: "host-record=${record}") cfg.dns.hostRecords
    ++ mapAttrsToList (domain: addr: "address=/${domain}/${addr}") cfg.dns.addressRecords;

  customDnsmasqConf =
    pkgs.writeText "99-pihole-custom.conf"
    (concatStringsSep "\n" dnsmasqExtraLines + "\n");

  hasCustomDnsmasqConf = dnsmasqExtraLines != [];
in {
  options.modules.linux.oci.services.pihole = {
    enable = mkEnableOption "Pi-hole DNS server";

    image = imageLib.mkImageOptions {
      repository = "pihole/pihole";
      version = "2026.06.0";
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
      ipv6Router = mkOption {
        description = ''
          Whether Pi-hole's IPv6 Router Advertisements announce this host as a
          default router. A DNS/DHCP appliance is normally NOT your gateway, so
          this defaults to false: RAs still carry the RDNSS DNS-server
          announcement (so IPv6 clients use Pi-hole for DNS, per RFC 8106), but
          the RA router lifetime is set to 0 so clients do not install this host
          as a default IPv6 route. Only enable if this host is genuinely your
          IPv6 gateway with working upstream IPv6 transit.
        '';
        type = types.bool;
        default = false;
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
      dnsServerV6 = mkOption {
        description = ''
          IPv6 DNS server advertised to clients via the RA RDNSS option and
          DHCPv6. Set this to your floating/HA DNS address (e.g. a keepalived
          VIP) rather than letting dnsmasq auto-select one of the host's own
          addresses — otherwise the advertised address can change across
          restarts and will not survive failover. Brackets are added
          automatically. Only takes effect when dhcp.ipv6 is enabled.
        '';
        type = types.nullOr types.str;
        default = null;
        example = "2600:1702:6710:117f::fe";
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
      hostRecords = mkOption {
        description = ''
          dnsmasq host-record values, rendered as "host-record=<value>".
          Statically-addressed hosts never appear in DHCP leases, so no
          lease-derived record exists for them — chiefly the Pi-hole hosts
          themselves. List the records for ALL DNS hosts on EVERY Pi-hole
          so names keep resolving across keepalived failover; a Pi-hole
          answers queries for its own hostname via FTL's self-hostname
          logic, but has no record at all for its peer without these.
        '';
        type = types.listOf types.str;
        default = [];
        example = ["pallas.lan,192.168.0.2" "hecate.lan,192.168.0.77"];
      };
      addressRecords = mkOption {
        description = ''
          Wildcard domain -> address map, rendered as
          "address=/<domain>/<addr>". Unlike host-record, an address
          entry matches the domain itself *and* every name beneath it,
          so a single entry points an entire subdomain tree at a
          reverse proxy instead of enumerating each name. More
          specific answers still win: a host-record, a DHCP lease, or
          a Pi-hole local DNS record for one name overrides the
          wildcard, which is how individual names get carved out.
          Only the listed family is answered — a v4 address leaves
          AAAA queries for those names as NODATA. List the same
          entries on EVERY Pi-hole so names survive failover.
        '';
        type = types.attrsOf types.str;
        default = {};
        example = {"home.lan" = "192.168.0.2";};
      };
    };

    ntpSync = mkOption {
      description = ''
        Let FTL's built-in NTP client steer the host clock (grants
        SYS_TIME). Off by default: NixOS hosts already run
        systemd-timesyncd, and a second clock-steering daemon adds a
        host-clock write capability for nothing. FTL's NTP *server*
        works either way.
      '';
      type = types.bool;
      default = false;
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
      image = imageLib.renderImage cfg.image;

      # Host network mode for DNS/DHCP
      extraOptions =
        [
          "--network=host"
        ]
        # Plain DNS serving needs none of these (NET_BIND_SERVICE is in
        # podman's default set). DHCP needs BOTH together: dnsmasq pings
        # a candidate lease (NET_RAW) and updates the ARP cache
        # (NET_ADMIN), and DHCPv6/RA opens a raw ICMPv6 socket needing
        # the pair. Never grant just one — the entrypoint only checks
        # NET_ADMIN, and FTL missing NET_RAW keeps the web UI alive with
        # DNS+DHCP silently dead (FTL v6.7 main.c catches dnsmasq's die()
        # and idles). Upstream's README omits NET_RAW only because
        # docker's default cap set has it; podman's does not.
        ++ optionals cfg.dhcp.enable [
          "--cap-add=NET_ADMIN"
          "--cap-add=NET_RAW"
        ]
        ++ optional cfg.ntpSync "--cap-add=SYS_TIME"
        ++ imageLib.mkImageLabels {
          module = "pihole";
          inherit (cfg) image;
        };

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
          # FTL answers queries for its own hostname itself (host-records
          # don't apply to it), and force4/force6 with an empty address
          # makes that answer 0.0.0.0. These keys live in pihole.toml on
          # the persistent volume, so a stray value survives redeploys
          # invisibly — a leftover force4=true black-holed pallas.lan for
          # every client. Env-pin auto-detection so each container start
          # re-asserts it.
          "FTLCONF_dns_reply_host_force4" = "false";
          "FTLCONF_dns_reply_host_force6" = "false";
          # Keep FTL's clock-sync intent in lockstep with the SYS_TIME
          # grant so it doesn't retry failing clock-set calls when the
          # cap is absent.
          "FTLCONF_ntp_sync_active" = boolToString cfg.ntpSync;
        }
        // optionalAttrs cfg.dhcp.enable {
          "FTLCONF_dhcp_active" = "true";
          "FTLCONF_dhcp_start" = cfg.dhcp.start;
          "FTLCONF_dhcp_end" = cfg.dhcp.end;
          "FTLCONF_dhcp_router" = cfg.dhcp.router;
          "FTLCONF_dhcp_ipv6" = boolToString cfg.dhcp.ipv6;
          "FTLCONF_dhcp_rapidCommit" = boolToString cfg.dhcp.rapidCommit;
        }
        // optionalAttrs hasCustomDnsmasqConf {
          "FTLCONF_misc_etc_dnsmasq_d" = "true";
        };

      volumes =
        [
          "${cfg.baseDir}/etc-pihole:/etc/pihole"
          # "${cfg.baseDir}/etc-dnsmasq.d:/etc/dnsmasq.d"
        ]
        ++ optional hasCustomDnsmasqConf
        "${customDnsmasqConf}:/etc/dnsmasq.d/99-pihole-custom.conf:ro";

      log-driver = "journald";
    };

    # Pi-hole uses host networking, so it doesn't need the normal network dependencies
    # but still wants to be part of the root target for lifecycle management
    systemd.services."podman-pihole" = {
      # See mkServiceConfig in default.nix: keep retrying through transient
      # startup failures rather than latching to the start-limit.
      startLimitIntervalSec = mkOverride 90 0;
      serviceConfig = {
        Restart = mkOverride 90 "always";
        RestartSec = mkOverride 90 10;
        # Podman does not create missing bind-mount sources (statfs error at
        # container start). _managedPaths/ZFS only materializes baseDir
        # itself, and pihole's own hosts (pallas/hecate) may not even have
        # oci.zfs enabled, so etc-pihole is created here unconditionally.
        ExecStartPre = ["${pkgs.coreutils}/bin/mkdir -p ${cfg.baseDir}/etc-pihole"];
      };
      partOf = ["${ociLib.rootTargetName}.target"];
      wantedBy = ["${ociLib.rootTargetName}.target"];
    };
  };
}
