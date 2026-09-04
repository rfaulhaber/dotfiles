{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.airvpn;

  netbirdEnabled = config.modules.services.netbird.enable;

  # netbird marks every socket it owns (wt0's encapsulated UDP, the STUN
  # shared socket, relay and management connections) with this value and
  # relies on `not fwmark` rules to keep them out of its own routing table.
  # NetworkManager's catch-all for the tunnel exempts only NM's own mark, so
  # without this rule netbird's packets would be re-tunnelled through AirVPN:
  # every toggle would re-negotiate every peer, and wt0's 1280-byte frames
  # would fragment inside a 1320-byte tunnel. Priority 100 sits ahead of
  # netbird's own rules (105/110) and far ahead of the pair NM derives from
  # the profile UUID (30768-31766), so netbird traffic consults the main
  # table first and leaves over the physical uplink.
  netbirdBypassRule = "priority 100 fwmark 0x1bd00 table 254";

  isV6 = hasInfix ":";
  v4Dns = filter (d: !isV6 d) cfg.dns;
  v6Dns = filter isV6 cfg.dns;

  # Keyfile lists are `;`-separated and `;`-terminated.
  keyfileList = concatMapStrings (x: "${x};");

  vpn = lib.my.writeNushellScriptBin pkgs "vpn" (
    # The script itself resolves nmcli from PATH and takes its default profile
    # from the environment, so it runs and debugs unchanged from bin/. This
    # preamble is all the packaged copy adds: the profile this module manages,
    # and nmcli for callers with a minimal PATH.
    ''
      $env.PATH = ($env.PATH | prepend "${config.networking.networkmanager.package}/bin")
      $env.VPN_PROFILE = ($env.VPN_PROFILE? | default "${cfg.profile}")
    ''
    + builtins.readFile "${config.dotfiles.binDir}/vpn.nu"
  );
in {
  options.modules.services.airvpn = {
    enable = mkEnableOption "on-demand AirVPN WireGuard tunnel";

    profile = mkOption {
      type = types.str;
      default = "airvpn";
      readOnly = true;
      description = ''
        NetworkManager connection id of the tunnel, which is also its
        interface name. Exposed so the desktop side can address the same
        profile.
      '';
    };

    endpoint = mkOption {
      type = types.str;
      example = "america3.vpn.airdns.org:1637";
      description = ''
        `host:port` of the AirVPN server, as printed by the config generator.
        The `*.vpn.airdns.org` names rotate across a pool; the digit selects
        the entry IP, and entry IP 3 is the one that carries IPv6 inside the
        tunnel.
      '';
    };

    publicKey = mkOption {
      type = types.str;
      default = "PyLCXAQT8KkM4T+dUsOQfn+Ub3pGxfGlxkIApuig+hk=";
      description = ''
        Server WireGuard public key. AirVPN uses a single key across its
        fleet; compare with the generator output if a handshake never
        completes.
      '';
    };

    mtu = mkOption {
      type = types.int;
      default = 1320;
      description = "Tunnel MTU. AirVPN's generator emits 1320.";
    };

    persistentKeepalive = mkOption {
      type = types.int;
      default = 15;
      description = "Seconds between keepalives, as emitted by the generator.";
    };

    dns = mkOption {
      type = types.listOf types.str;
      default = ["10.128.0.1" "fd7d:76ee:e68f:a993::1"];
      description = ''
        Resolvers that take every query not owned by another link's routing
        domain while the tunnel is up; AirVPN's in-tunnel resolvers by
        default. An empty list leaves the LAN resolver in charge, at the cost
        of DNS traffic leaving outside the tunnel.
      '';
    };

    ipv6 = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Carry IPv6 through the tunnel as well. Needs the `airvpn/address6`
        secret and an endpoint that offers IPv6 inside the tunnel.
      '';
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.networking.networkmanager.enable;
        message = "modules.services.airvpn drives the tunnel through NetworkManager; enable networking.networkmanager";
      }
    ];

    sops.secrets =
      {
        "airvpn/private-key" = {};
        "airvpn/preshared-key" = {};
        # The assigned tunnel address is per-device identifying material.
        "airvpn/address4" = {};
      }
      // optionalAttrs cfg.ipv6 {
        "airvpn/address6" = {};
      };

    # Substituted into the profile below by NetworkManager-ensure-profiles,
    # which writes the result under /run with mode 0600.
    sops.templates."airvpn-nm.env".content =
      ''
        AIRVPN_PRIVATE_KEY=${config.sops.placeholder."airvpn/private-key"}
        AIRVPN_PRESHARED_KEY=${config.sops.placeholder."airvpn/preshared-key"}
        AIRVPN_ADDRESS4=${config.sops.placeholder."airvpn/address4"}
      ''
      + optionalString cfg.ipv6 ''
        AIRVPN_ADDRESS6=${config.sops.placeholder."airvpn/address6"}
      '';

    networking.networkmanager.ensureProfiles = {
      environmentFiles = [config.sops.templates."airvpn-nm.env".path];
      profiles.${cfg.profile} = {
        connection = {
          id = cfg.profile;
          type = "wireguard";
          interface-name = cfg.profile;
          # Never comes up on its own: after a reboot the tunnel stays down
          # until asked for, and a rebuild neither starts nor stops it.
          autoconnect = false;
        };
        wireguard = {
          inherit (cfg) mtu;
          private-key = "$AIRVPN_PRIVATE_KEY";
        };
        "wireguard-peer.${cfg.publicKey}" =
          {
            inherit (cfg) endpoint;
            preshared-key = "$AIRVPN_PRESHARED_KEY";
            # Without an explicit 0 the key counts as "not required" and is
            # dropped from the profile.
            preshared-key-flags = 0;
            # A /0 here is what makes NM install the fwmark-routed default
            # route in a private table, leaving LAN and netbird prefixes in
            # main untouched.
            allowed-ips = keyfileList (["0.0.0.0/0"] ++ optional cfg.ipv6 "::/0");
          }
          // optionalAttrs (cfg.persistentKeepalive > 0) {
            persistent-keepalive = cfg.persistentKeepalive;
          };
        ipv4 =
          {
            method = "manual";
            address1 = "$AIRVPN_ADDRESS4";
          }
          // optionalAttrs (v4Dns != []) {
            dns = keyfileList v4Dns;
          }
          // optionalAttrs netbirdEnabled {
            routing-rule1 = netbirdBypassRule;
          };
        ipv6 =
          if cfg.ipv6
          then
            {
              method = "manual";
              address1 = "$AIRVPN_ADDRESS6";
              # A keyfile without this key falls back to EUI-64, which derives
              # the link-local address from a MAC the WireGuard device does
              # not have; NM then logs "IPv6 cannot continue" and never
              # finishes configuring the family.
              addr-gen-mode = "stable-privacy";
            }
            // optionalAttrs (v6Dns != []) {
              dns = keyfileList v6Dns;
            }
            // optionalAttrs netbirdEnabled {
              routing-rule1 = netbirdBypassRule;
            }
          else {
            method = "disabled";
          };
      };
    };

    # Reverse-path filtering only honours the fwmark on the reply path with
    # this set; wg-quick flips it when it installs a marked default route,
    # NetworkManager does not.
    boot.kernel.sysctl."net.ipv4.conf.all.src_valid_mark" = 1;

    # The firewall's strict rpfilter drops the tunnel's own handshake replies:
    # they arrive unmarked on the uplink, and the unmarked reverse path for
    # the endpoint's address is the catch-all route into the tunnel. Loose
    # mode still refuses sources with no route at all.
    networking.firewall.checkReversePath = "loose";

    user.packages = [
      vpn
      # `sudo wg show airvpn` for handshake and transfer counters, which NM
      # does not expose.
      pkgs.wireguard-tools
    ];
  };
}
