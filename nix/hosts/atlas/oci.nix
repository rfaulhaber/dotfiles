{
  config,
  lib,
  ...
}: {
  modules.linux.oci = {
    enable = true;

    zfs = {
      enable = true;
      pool = "data";
    };

    registryAuth = {
      enable = true;
      # The forgejo container registry on this host, via its published
      # web port; the secret is base64("<user>:<PAT with read:packages>").
      registries."localhost:2835".secret = "registry-auth/forgejo";
    };

    # Image versions/digests come from oci-images.json so an
    # auto-update workflow can rewrite plain JSON instead of nix.
    services =
      lib.importJSON ./oci-images.json
      |> lib.recursiveUpdate {
        gluetun = {
          enable = true;
          baseDir = "/data/apps/gluetun";
          extraPorts = [
            "8888:8888/tcp" # HTTP proxy
            "8388:8388/tcp" # Shadowsocks
            "8388:8388/udp" # Shadowsocks
          ];
        };

        transmission = {
          enable = true;
          baseDir = "/data/apps/transmission";
          downloadsDir = "/data/transmission";
          useGluetun = true;
          ratioLimit = 2.0;
          ratioLimitEnabled = true;
          idleSeedingLimit = 30;
          idleSeedingLimitEnabled = true;
          peerLimits = {
            global = 200;
            perTorrent = 50;
          };
          uploadSlotsPerTorrent = 14;
        };

        flaresolverr = {
          enable = true;
          useGluetun = true;
        };

        prowlarr = {
          enable = true;
          baseDir = "/data/apps/prowlarr";
          useGluetun = true;
          dependsOn = ["flaresolverr"];
        };

        radarr = {
          enable = true;
          baseDir = "/data/apps/radarr";
          useGluetun = true;
          dependsOn = ["prowlarr" "transmission"];
          mediaDirs = {
            movies = "/data/movies";
            "downloads/transmission" = "/data/transmission";
            "downloads/nzb" = "/data/nzb";
          };
        };

        sonarr = {
          enable = true;
          baseDir = "/data/apps/sonarr";
          useGluetun = true;
          dependsOn = ["prowlarr" "transmission"];
          mediaDirs = {
            tv = "/data/tv";
            "downloads/transmission" = "/data/transmission";
            "downloads/nzb" = "/data/nzb";
          };
        };

        bazarr = {
          enable = true;
          baseDir = "/data/apps/bazarr";
          useGluetun = true;
          dependsOn = ["radarr" "sonarr"];
          mediaDirs = {
            movies = "/data/movies";
            tv = "/data/tv";
          };
        };

        nzbget = {
          enable = true;
          baseDir = "/data/apps/nzbget";
          downloadsDir = "/data/nzb";
          useGluetun = true;
        };

        slskd = {
          enable = true;
          baseDir = "/data/apps/slskd";
          downloadsDir = "/data/slskd";
          musicDir = "/data/music";
          useGluetun = true;
          apiKeys.soularr.secretName = "slskd/soularr-api-key";
        };

        lidarr = {
          enable = true;
          baseDir = "/data/apps/lidarr";
          useGluetun = true;
          dependsOn = ["prowlarr" "transmission" "slskd"];
          mediaDirs = {
            music = "/data/music";
            "downloads/transmission" = "/data/transmission";
            "downloads/slskd" = "/data/slskd";
          };
        };

        soularr = {
          enable = true;
          baseDir = "/data/apps/soularr";
          slskdDownloadsDir = "/data/slskd";
          useGluetun = true;
        };

        requestrr = {
          enable = true;
          baseDir = "/data/apps/requestrr";
          useGluetun = true;
          discord = {
            monitoredChannelSecrets = ["requestrr/discord-channel-main"];
            notificationChannelSecrets = ["requestrr/discord-channel-main"];
          };
          radarr = {
            enable = true;
            hostname = "gluetun";
            apiKeySecret = "recyclarr/radarr-main-api-key";
            categories = [
              {
                Id = 0;
                Name = "movie";
                # Radarr-instance-local quality profile id; 8 is the
                # recyclarr-managed "HD Bluray + WEB".
                ProfileId = 8;
                RootFolder = "/movies";
                MinimumAvailability = "released";
                Tags = [];
              }
            ];
          };
          sonarr = {
            enable = true;
            hostname = "gluetun";
            apiKeySecret = "recyclarr/sonarr-main-api-key";
            categories = [
              {
                Id = 0;
                Name = "tv";
                # Sonarr-instance-local quality profile id; 8 is the
                # recyclarr-managed "WEB-1080p (Alternative)".
                ProfileId = 8;
                RootFolder = "/tv";
                Tags = [];
                LanguageId = 1;
                UseSeasonFolders = true;
                SeriesType = "standard";
              }
            ];
          };
        };

        recyclarr = {
          enable = true;
          baseDir = "/data/apps/recyclarr";
          useGluetun = true;
        };

        immich = {
          enable = true;
          baseDir = "/data/apps/immich";
          filesEncryption = {
            enable = true;
            keyFile = ./secrets/immich-zfs-key;
          };
          machineLearning = {
            enable = false;
            # atlas delegates ML to vulcan instead of running a local sidecar.
            url = "http://vulcan.lan:3003";
          };
        };

        miniflux = {
          enable = true;
          baseDir = "/data/apps/miniflux";
          postgres.pgdata = "/var/lib/postgresql/data/18/docker";
          oidc = {
            enable = true;
            discoveryEndpoint = "https://auth.3679.space";
            redirectUrl = "https://rss.3679.space/oauth2/oidc/callback";
            providerName = "PocketID";
            userCreation = true;
          };
        };

        forgejo = {
          enable = true;
          baseDir = "/data/apps/forgejo";
          domain = "git.3679.space";
          rootUrl = "https://git.3679.space";
          sshDomain = "git.3679.space";
          postgres.port = 8256;
          # Logins go through PocketID, so the reverse-proxy auth header
          # stays off (module default) and this only governs which peers
          # may set X-Forwarded-For: newt, from the podman nets, and
          # pallas' caddy. Narrow it to newt's address once observed.
          trustedProxies = ["10.89.0.0/16" "192.168.0.2/32"];
        };

        calibre = {
          enable = true;
          baseDir = "/data/apps/calibre";
          booksDir = "/data/books";
        };

        calibre-web-auto = {
          enable = true;
          baseDir = "/data/apps/calibre-web";
          libraryDir = "/data/books";
          ingestDir = "/data/books/cwa-book-ingest";
        };

        filebrowser = {
          enable = true;
          baseDir = "/data/apps/filebrowser";
          filesDir = "/data/apps/filebrowser/files";
          oidc = {
            enable = true;
            issuerUrl = "https://auth.3679.space";
          };
        };

        linkding = {
          enable = true;
          baseDir = "/data/apps/linkding";
          oidc = {
            enable = true;
            authorizationEndpoint = "https://auth.3679.space/authorize";
            tokenEndpoint = "https://auth.3679.space/api/oidc/token";
            userEndpoint = "https://auth.3679.space/api/oidc/userinfo";
            jwksEndpoint = "https://auth.3679.space/.well-known/jwks.json";
          };
        };

        navidrome = {
          enable = true;
          baseDir = "/data/apps/navidrome";
          musicDir = "/data/music";
          lastfm.enable = true;
        };

        paperless = {
          enable = true;
          baseDir = "/data/apps/paperless";
          url = "https://paperless.3679.space";
          dataEncryption = {
            enable = true;
            keyFile = ./secrets/paperless-data-zfs-key;
          };
          mediaEncryption = {
            enable = true;
            keyFile = ./secrets/paperless-media-zfs-key;
          };
          dbEncryption = {
            enable = true;
            keyFile = ./secrets/paperless-db-zfs-key;
          };
          oidc = {
            enable = true;
            serverUrl = "https://auth.3679.space/.well-known/openid-configuration";
          };
        };

        job-ops = {
          enable = true;
          baseDir = "/data/apps/job-ops";
          publicBaseUrl = "https://jobs.3679.space";
        };

        trading-bot = {
          enable = true;
          baseDir = "/data/apps/trading-bot";
          paper.enable = true;
        };

        syncthing = {
          enable = true;
          baseDir = "/data/apps/syncthing";
          syncDirs = {
            data = "/data/sync";
            "data/org" = "/data/org";
          };
        };

        tautulli = {
          enable = true;
          baseDir = "/data/apps/tautulli";
        };

        kitchenowl = {
          enable = true;
          baseDir = "/data/apps/kitchenowl";
          frontUrl = "https://kitchenowl.3679.space";
          oidc = {
            enable = true;
            issuer = "https://auth.3679.space";
          };
        };

        vikunja = {
          enable = true;
          baseDir = "/data/apps/vikunja";
          publicUrl = "https://tasks.3679.space";
          auth.openid = {
            enable = true;
            redirectUrl = "https://tasks.3679.space/auth/openid/pocketid";
            providers.PocketID = {
              displayName = "Pocket ID";
              authUrl = "https://auth.3679.space";
            };
          };
        };

        prometheus = {
          enable = true;
          baseDir = "/data/apps/prometheus";
          openFirewall = true;
          extraScrapeConfigs = [
            {
              job_name = "node";
              static_configs = [
                {
                  # atlas self-scrape uses podman's host-gateway DNS to avoid
                  # a circular dependency through the local pi-hole.
                  targets = ["host.containers.internal:9100"];
                  labels.host = "atlas";
                }
                {
                  targets = ["vulcan.lan:9100"];
                  labels.host = "vulcan";
                }
                {
                  targets = ["pallas.lan:9100"];
                  labels.host = "pallas";
                }
                {
                  targets = ["hecate.lan:9100"];
                  labels.host = "hecate";
                }
                {
                  targets = ["prometheus.lan:9100"];
                  labels.host = "prometheus";
                }
                {
                  # janus is a cloud VPS — no LAN DNS, reached over the
                  # netbird overlay. Resolution works inside this container
                  # because podman inherits the host's resolver.
                  targets = ["janus.netbird.selfhosted:9100"];
                  labels.host = "janus";
                }
              ];
              # Copy the explicit `host` label onto `instance` so community
              # node-exporter dashboards (which template on `instance`) show
              # readable host names instead of `vulcan.lan:9100`.
              relabel_configs = [
                {
                  source_labels = ["host"];
                  target_label = "instance";
                }
              ];
            }
            {
              job_name = "podman-exporter";
              static_configs = [
                {
                  # atlas's podman-exporter shares the observability network
                  # with prometheus, so the container alias resolves directly —
                  # no host port hop, no firewall dependency.
                  targets = ["podman-exporter:9882"];
                  labels.host = "atlas";
                }
                {
                  targets = ["vulcan.lan:9882"];
                  labels.host = "vulcan";
                }
                {
                  targets = ["janus.netbird.selfhosted:9882"];
                  labels.host = "janus";
                }
              ];
              relabel_configs = [
                {
                  source_labels = ["host"];
                  target_label = "instance";
                }
              ];
            }
          ];
        };

        loki = {
          enable = true;
          baseDir = "/data/apps/loki";
          openFirewall = true;
        };

        podman-exporter = {
          enable = true;
          # Prometheus scrapes via the shared observability network alias,
          # so the host-port publish is unused here — pin it to loopback.
          # Without a bindAddress podman publishes on 0.0.0.0 through a
          # PREROUTING DNAT the NixOS firewall never sees, so openFirewall
          # = false alone does not keep :9882 off the LAN.
          bindAddress = "127.0.0.1";
        };

        grafana = {
          enable = true;
          baseDir = "/data/apps/grafana";
          openFirewall = true;
          # https since pallas's caddy serves this name with tls internal;
          # the PocketID client's redirect URI must match the https form.
          rootUrl = "https://grafana.home.lan";
          dashboardsPath = ./dashboards;
          oidc = {
            enable = true;
            issuerUrl = "https://auth.3679.space";
            providerName = "PocketID";
            adminGroup = "admin";
          };
        };

        sure = {
          enable = true;
          baseDir = "/data/apps/sure";
          appUrl = "https://sure.3679.space";
          oidc = {
            enable = true;
            issuer = "https://auth.3679.space";
          };
          onboardingState = "closed";
          localLogin.enabled = false;
          dbEncryption = {
            enable = true;
            keyFile = ./secrets/sure-db-zfs-key;
          };
          storageEncryption = {
            enable = true;
            keyFile = ./secrets/sure-storage-zfs-key;
          };
        };

        newt = {
          enable = true;
          pangolinEndpoint = "https://pangolin.3679.space";
          dns = "192.168.0.2";
          networks = ["default" "immich" "forgejo" "sure" "paperless" "miniflux"];
          # The filtered proxy socket, never the real one — direct access is
          # root-equivalent, and a compromise of newt (or of the Pangolin
          # control plane on janus that it trusts) would reach root on atlas.
          dockerSocket = "unix:///var/run/docker.sock";
          hostSocket = config.modules.services.docker-socket-proxy.socketPath;
        };
      };
  };

  # Read-only container visibility for newt's target picker: list containers
  # and watch start/stop, nothing else.
  modules.services.docker-socket-proxy = {
    enable = true;
    allowedApiSections = ["containers" "events"];
  };

  # Ordering only — the bind-mount source must exist when podman-newt starts
  # (podman errors on a missing volume source rather than creating it).
  # Deliberately no Requires=: the picker is optional, the tunnel is not, and
  # if the proxy is down newt's Restart=always retries cover the gap.
  systemd.services."podman-newt".after = ["docker-socket-proxy.service"];
}
