{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.keepalived;

  healthCheckScript = pkgs.writeShellScript "check-dns" ''
    ${pkgs.dig}/bin/dig @127.0.0.1 +short +time=2 +tries=1 google.com > /dev/null 2>&1
  '';
in {
  options.modules.services.keepalived = {
    enable = mkEnableOption "keepalived VRRP failover";

    interface = mkOption {
      type = types.str;
      description = "Network interface for VRRP communication.";
      example = "end0";
    };

    state = mkOption {
      type = types.enum ["MASTER" "BACKUP"];
      description = "Initial VRRP state.";
    };

    priority = mkOption {
      type = types.int;
      description = "VRRP priority (higher wins the election).";
      example = 100;
    };

    virtualIps = mkOption {
      type = types.listOf types.str;
      description = "Virtual IP addresses in CIDR notation.";
      example = ["192.168.0.254/24"];
    };

    virtualIpv6s = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Virtual IPv6 addresses in CIDR notation. When non-empty, creates a separate VRRP instance for IPv6.";
      example = ["2600:1702:6710:117F::FE/64"];
    };

    virtualRouterId = mkOption {
      type = types.int;
      default = 51;
      description = "VRRP virtual router ID (must match across all peers).";
    };

    virtualRouterId6 = mkOption {
      type = types.int;
      default = 61;
      description = "VRRP virtual router ID for IPv6 (must match across all peers and differ from the IPv4 ID).";
    };

    authPass = mkOption {
      type = types.str;
      description = "VRRP authentication password (max 8 characters).";
    };

    healthCheck = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable DNS health check that triggers failover when pihole is unresponsive.";
      };
      interval = mkOption {
        type = types.int;
        default = 5;
        description = "Seconds between health checks.";
      };
      fall = mkOption {
        type = types.int;
        default = 3;
        description = "Consecutive failures before marking unhealthy.";
      };
      rise = mkOption {
        type = types.int;
        default = 2;
        description = "Consecutive successes before marking healthy.";
      };
    };
  };

  config = mkIf cfg.enable {
    # keepalived refuses to run check scripts as root; it expects this user
    users.users.keepalived_script = mkIf cfg.healthCheck.enable {
      isSystemUser = true;
      group = "keepalived_script";
    };
    users.groups.keepalived_script = mkIf cfg.healthCheck.enable {};

    # VRRPv3 (IPv6) requires a link-local address on the interface.
    # NixOS sets addr_gen_mode=1 (none) on statically-configured interfaces,
    # which suppresses auto-generation of fe80:: addresses. Reset to EUI-64.
    boot.kernel.sysctl = mkIf (cfg.virtualIpv6s != []) {
      "net.ipv6.conf.${cfg.interface}.addr_gen_mode" = 0;
    };

    services.keepalived = {
      enable = true;

      vrrpScripts = mkIf cfg.healthCheck.enable {
        check_dns = {
          script = toString healthCheckScript;
          interval = cfg.healthCheck.interval;
          weight = -20;
          fall = cfg.healthCheck.fall;
          rise = cfg.healthCheck.rise;
        };
      };

      vrrpInstances =
        {
          VI_DNS = {
            interface = cfg.interface;
            state = cfg.state;
            priority = cfg.priority;
            virtualRouterId = cfg.virtualRouterId;
            virtualIps = map (addr: {inherit addr;}) cfg.virtualIps;
            trackScripts = optional cfg.healthCheck.enable "check_dns";
            extraConfig = ''
              authentication {
                auth_type PASS
                auth_pass ${cfg.authPass}
              }
              advert_int 1
            '';
          };
        }
        // optionalAttrs (cfg.virtualIpv6s != []) {
          VI_DNS6 = {
            interface = cfg.interface;
            state = cfg.state;
            priority = cfg.priority;
            virtualRouterId = cfg.virtualRouterId6;
            virtualIps = map (addr: {inherit addr;}) cfg.virtualIpv6s;
            trackScripts = optional cfg.healthCheck.enable "check_dns";
            # VRRP v3 (required for IPv6) does not support authentication
            extraConfig = ''
              advert_int 1
            '';
          };
        };
    };

    # VRRP uses IP protocol 112 — not a TCP/UDP port
    networking.firewall.extraCommands =
      ''
        iptables -I INPUT -p vrrp -j ACCEPT
      ''
      + optionalString (cfg.virtualIpv6s != []) ''
        ip6tables -I INPUT -p vrrp -j ACCEPT
      '';
  };
}
