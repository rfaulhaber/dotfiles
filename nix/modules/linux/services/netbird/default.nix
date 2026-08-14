{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.netbird;
in {
  options.modules.services.netbird = {
    enable = mkEnableOption false;
    autoStart = mkOption {
      type = types.bool;
      default = true;
      description = "Whether or not to auto start the default netbird interface.";
    };
    manageDNS = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether Netbird manages system DNS by rewriting /etc/resolv.conf to
        point at its embedded resolver.

        Set this to false on a host that is itself the network's Pi-hole
        resolver. Netbird takes over resolv.conf exclusively, replacing the
        declared `networking.nameservers` with a single entry pointing at its
        own resolver — which ultimately depends on Pi-hole. When the Pi-hole
        container briefly stops (e.g. mid-activation during an image-bump
        deploy) the host is left with no working resolver, so outbound lookups
        — including the container image pull itself — fail and deadlock the
        deploy. Disabling DNS management keeps NixOS in control of resolv.conf
        so the declared non-Pi-hole fallback survives Pi-hole downtime.
      '';
    };
    setupKeyFile = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = literalExpression ''config.sops.secrets."netbird/setup-key".path'';
      description = ''
        Path to a file containing a Netbird Setup Key. When set, the client
        auto-enrolls and connects to the network on first boot via a sibling
        `netbird-login.service` (no manual `netbird up` required).
      '';
    };
  };

  config = mkIf cfg.enable {
    services.netbird = {
      enable = true;
      clients.default = {
        inherit (cfg) autoStart;
        # Point the client at the self-hosted Netbird control plane. Merged
        # into /var/lib/netbird/config.json at preStart, so the daemon enrolls
        # against the right management server (default would be api.netbird.io).
        #
        # ManagementURL/AdminURL are typed `*url.URL` in netbird's Config; Go's
        # encoding/json serializes them as nested objects and cannot unmarshal
        # a plain string here. Match the on-disk shape netbird itself writes.
        # The management API is exposed on :33073 by the OCI module
        # (nix/modules/linux/oci/netbird.nix); the dashboard is on :443.
        config =
          {
            ManagementURL = {
              Scheme = "https";
              Host = "netbird.3679.space:33073";
            };
            AdminURL = {
              Scheme = "https";
              Host = "netbird.3679.space:443";
            };
          }
          # On a Pi-hole host (manageDNS = false) Netbird must stay off system
          # DNS entirely. DisableDNS stops it rewriting resolv.conf, but its
          # embedded resolver still binds <netbird-ip>:53 — which collides with
          # Pi-hole's wildcard 0.0.0.0:53 bind (dns.listeningMode = SINGLE),
          # losing the port-53 race on restart and silently taking Pi-hole's
          # DNS resolver down. Pinning Netbird's resolver to a loopback high
          # port keeps port 53 free for Pi-hole. (netbird >=0.74 no longer frees
          # the socket via DisableDNS alone.)
          // optionalAttrs (!cfg.manageDNS) {
            DisableDNS = true;
            CustomDNSAddress = "127.0.0.1:5053";
          };
        login = mkIf (cfg.setupKeyFile != null) {
          enable = true;
          inherit (cfg) setupKeyFile;
          systemdDependencies =
            lib.optional config.modules.programs.sops.enable
            "sops-install-secrets.service";
        };
      };
    };

    user.packages = with pkgs;
      [
        netbird
      ]
      ++ lib.optionals config.modules.desktop.enable [
        netbird-ui
      ];
  };
}
