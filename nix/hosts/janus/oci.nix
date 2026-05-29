{lib, ...}: let
  inherit (lib.importJSON ./ips.json) pangolin netbird;
  pangolinIp = pangolin;
  netbirdIp = netbird;
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
          bindAddress = pangolinIp;
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
          # Don't enforce the CrowdSec community blocklist: it inherits all
          # of CAPI's false positives (banned residential ISPs, mobile carrier
          # ranges, VPN exits) and we don't know our Pangolin users' IPs in
          # advance, so we can't safely allowlist them. Local detection keeps
          # working — actual brute force / probing against janus still triggers
          # the configured scenarios and bans the source.
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

        podman-exporter.enable = true;
      };
  };
}
