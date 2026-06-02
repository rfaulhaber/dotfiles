{lib, ...}: let
  inherit (lib.importJSON ./ips.json) pangolin netbird netbirdOverlay;
  pangolinIp = pangolin;
  netbirdIp = netbird;
  # janus's address on the netbird overlay (wt0). Used to keep host-published
  # container ports off the public ens3 addresses — see podman-exporter below.
  netbirdOverlayIp = netbirdOverlay;
in {
  modules.linux.oci = {
    enable = true;

    networks = {
      pangolin.enable = true;
      netbird.enable = true;
    };

    services =
      lib.importJSON ./oci-images.json
      |> lib.recursiveUpdate {
        pangolin = {
          enable = true;
          domain = "3679.space";
          dashboardDomain = "pangolin.3679.space";
          bindAddress = pangolinIp;
          baseDir = "/docker/config";
          adminEmail = "ryf@sent.as";
          email = {
            smtpHost = "smtp.fastmail.com";
            smtpPort = 465;
            smtpUser = "ryf@sent.as";
            noReply = "no-reply@3679.space";
          };
          openFirewall = true;
          crowdsec.enable = true;
        };

        netbird = {
          enable = true;
          domain = "netbird.3679.space";
          authDomain = "auth.3679.space";
          bindAddress = netbirdIp;
          baseDir = "/docker/config/netbird";
          acmeEmail = "ryf@sent.as";
          openFirewall = true;
        };

        pocket-id = {
          enable = true;
          appUrl = "https://auth.3679.space";
          # Loopback-only host publish. Pocket-ID is reached as
          # https://auth.3679.space *through Traefik*, which routes to the
          # container by its podman alias (http://pocket-id:1411) over the
          # shared `pangolin` network — that path is unaffected by the host
          # bind. Publishing on a public ens3 IP only created a second
          # ingress that bypassed Traefik (and the CrowdSec bouncer). Keep a
          # loopback publish for local debugging; nothing external needs it.
          bindAddress = "127.0.0.1";
          baseDir = "/docker/config/pocket-id";
          networks = ["pangolin"];
        };

        crowdsec = {
          enable = true;
          baseDir = "/docker/config/crowdsec";
          networks = ["pangolin"];
          # Make sure traefik has had a chance to write its log file before
          # crowdsec tails it.
          dependsOn = ["traefik"];
          acquisitions.traefik = {
            type = "traefik";
            hostPath = "/docker/config/traefik/access.log";
          };
          bouncers.traefik.sopsKey = "crowdsec/bouncer-api-key";
          enroll = {
            enable = true;
            instanceName = "janus";
          };
          # Enforce the CrowdSec community blocklist (CAPI). Pulls the
          # community-curated blocklist for enforcement and shares local
          # detections back. Accepts the tradeoff that CAPI can occasionally
          # flag shared/residential/mobile ranges; tune via allowlist.cidrs if
          # a legitimate user IP gets caught.
          communityBlocklist.enable = true;

          # LePresidente/http-generic-403-bf is a community scenario that
          # fires on ~6 HTTP 403s in a short window. Two reasons to disable:
          #   1. Feedback loop: the bouncer's own ban response is 403, so a
          #      single already-banned IP retrying for 60s (the stream-mode
          #      cache TTL) generates >threshold events and creates a fresh
          #      *local* ban on top of whatever caused the original block.
          #   2. Organic false positives: badger session-expiry, multi-tab
          #      navigation to permission-gated resources, and browser asset
          #      preload misses all rack up 403s under normal use.
          # Auth brute force is still covered by crowdsecurity/http-bf (which
          # watches 401s) and the rest of base-http-scenarios.
          disabledHubItems = ["scenarios/LePresidente/http-generic-403-bf"];
        };

        podman-exporter = {
          enable = true;
          # Pin the publish to the netbird overlay IP. podman's host-port DNAT
          # bypasses the NixOS firewall, so the `firewall.interfaces.wt0`
          # narrowing in configuration.nix does NOT cover this container —
          # without a bind address it was reachable on the public ens3 IPs.
          # atlas scrapes it at janus.netbird.selfhosted:9882, which resolves
          # to this overlay address.
          bindAddress = netbirdOverlayIp;
        };
      };
  };
}
