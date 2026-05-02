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
      };

      recyclarr = {
        enable = true;
        baseDir = "/data/apps/recyclarr";
        useGluetun = true;
      };

      immich = {
        enable = true;
        baseDir = "/data/apps/immich";
        # The files dataset (photo uploads) is encrypted; key is sops-managed
        # and unlocked in stage 2 by zfs-load-key-immich.service. The dataset
        # itself was originally created out-of-band on the legacy stack with
        # encryption=aes-256-gcm, keyformat=raw — those properties are
        # create-only, so they ride along with the dataset; this declaration
        # ensures a clean rebuild reproduces the same encryption settings.
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
        # Legacy data is nested at <baseDir>/18/docker/PG_VERSION; override
        # PGDATA so postgres finds it instead of trying to initdb at the
        # bind-mount root and erroring with "directory is not empty".
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
        filesDir = "/data/filebrowser/files";
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
        configFile = "/data/apps/vikunja/config.yml";
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
