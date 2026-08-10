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
          # flag shared/residential/mobile ranges; household IPs are shielded
          # by the centralized allowlist below (parser-level allowlist.cidrs
          # cannot filter CAPI decisions).
          communityBlocklist.enable = true;

          # LAPI-level never-ban list covering every decision source — local
          # scenarios AND CAPI. The home WAN IP lives in sops because it
          # identifies a residence; the hook reads it inside the container.
          # If the ISP reassigns the home IP, update the secret and restart
          # podman-crowdsec after deploying.
          allowlist.centralized.sopsKey = "crowdsec/allowlist-cidrs";

          # LePresidente/http-generic-403-bf is a community scenario that
          # fires on ~6 HTTP 403s in a short window. Two reasons to suppress:
          #   1. Feedback loop: the bouncer's own ban response is 403, so a
          #      single already-banned IP retrying for 60s (the stream-mode
          #      cache TTL) generates >threshold events and creates a fresh
          #      *local* ban on top of whatever caused the original block.
          #   2. Organic false positives: badger session-expiry, multi-tab
          #      navigation to permission-gated resources, and browser asset
          #      preload misses all rack up 403s under normal use.
          # It is a sub-document bundled inside the crowdsecurity/
          # http-generic-bf hub file, not a standalone hub item, so
          # disabledHubItems can never remove it (that removal failed on
          # every boot for months); simulation matches the loaded scenario
          # name instead, and the LAPI hides simulated decisions from
          # bouncers. Auth brute force is still covered by the bundle's
          # 401-oriented siblings and the rest of base-http-scenarios.
          simulatedScenarios = ["LePresidente/http-generic-403-bf"];

          # The Newt tunnel agents on atlas and vulcan reach Pangolin from the
          # home network's single public WAN IP — the same address a browser
          # uses. Newt POSTs /api/v1/auth/newt/get-token and, whenever that IP
          # is already bounced, the bouncer's own 403 response feeds 403-counting
          # scenarios; Newt then retries every few seconds forever, pinning a
          # self-renewing ban on the whole household. Whitelisting the tunnel
          # control-plane path stops that traffic from ever scoring as an attack.
          # Safe because the endpoint requires a valid Newt token, so it isn't a
          # meaningful brute-force surface. IP-agnostic, so it survives the home
          # IP changing.
          allowlist.expressions = [
            "evt.Meta.log_type in ['http_access-log', 'http_error-log'] && evt.Meta.http_path startsWith '/api/v1/auth/newt/'"
            # Forgejo's container registry on git.3679.space: podman
            # push/pull probes blob existence with requests that 404 on
            # missing layers — 11 of those in one push tripped
            # crowdsecurity/http-probing. Forgejo enforces its own auth on
            # /v2/, so exempting the whole registry protocol from scenario
            # scoring is safe, and it protects any client IP (laptops away
            # from home, CI) rather than just the allowlisted home address.
            "evt.Meta.log_type == 'http_access-log' && evt.Meta.target_fqdn == 'git.3679.space' && evt.Meta.http_path startsWith '/v2/'"
            # Vikunja clients hammer POST /api/v1/user/token/refresh when a
            # session expires (6x 401 in 10s), tripping
            # LePresidente/http-generic-401-bf. Refresh requires a valid JWT
            # cookie, so the endpoint is not a password-guessing surface;
            # real credential brute force on /login stays detectable.
            "evt.Meta.log_type == 'http_access-log' && evt.Meta.target_fqdn == 'tasks.3679.space' && evt.Meta.http_path == '/api/v1/user/token/refresh'"
          ];
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
