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

    services = {
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
        # The named API key is shared with soularr — both modules point
        # at the same sops secret so rotating it updates both at once.
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
        # Preserve the live placeholder values from the legacy on-disk
        # config.ini for behavioral continuity. These look like template
        # examples and can be cleaned up later.
        searchSettings = {
          ignoredUsers = ["User1" "User2" "Fred" "Bob"];
          titleBlacklist = ["Word1" "word2"];
        };
      };

      requestrr = {
        enable = true;
        baseDir = "/data/apps/requestrr";
        useGluetun = true;
        # Identifying values (Discord IDs, RPC username) all live in sops.
        # Module defaults already point at the standard secret names, so
        # this block is mostly empty — we just point at the channel secret.
        discord = {
          monitoredChannelSecrets = ["requestrr/discord-channel-main"];
          notificationChannelSecrets = ["requestrr/discord-channel-main"];
        };
        # Reuse the same *arr API keys recyclarr uses — single source of
        # truth per *arr instance.
        radarr = {
          enable = true;
          hostname = "gluetun";
          apiKeySecret = "recyclarr/radarr-main-api-key";
          categories = [
            {
              Id = 0;
              Name = "movie";
              ProfileId = 1;
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
              ProfileId = 1;
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
          # Atlas delegates ML to vulcan instead of running a local sidecar.
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
        # Wire scrape targets here as agents come online on each host.
        # Example for atlas itself once you add `services.prometheus.exporters.node`:
        # extraScrapeConfigs = [{
        #   job_name = "node-atlas";
        #   static_configs = [{
        #     targets = ["host.containers.internal:9100"];
        #     labels.host = "atlas";
        #   }];
        # }];
      };

      loki = {
        enable = true;
        baseDir = "/data/apps/loki";
        openFirewall = true;
      };

      grafana = {
        enable = true;
        baseDir = "/data/apps/grafana";
        openFirewall = true;
        # LAN-only access. Update to match how you reach atlas from
        # your browser; the OIDC redirect URI is derived from this and
        # must be registered exactly in PocketID.
        rootUrl = "http://atlas.lan:3000";
        oidc = {
          enable = true;
          issuerUrl = "https://auth.3679.space";
          providerName = "PocketID";
          adminGroup = "admin";
        };
      };

      newt = {
        enable = true;
        pangolinEndpoint = "https://pangolin.3679.space";
        dns = "192.168.0.2";
        networks = ["default" "immich" "forgejo"];
      };
    };
  };
}
