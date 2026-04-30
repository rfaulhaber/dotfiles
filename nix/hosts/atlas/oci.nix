{
  config,
  lib,
  ...
}: {
  modules.programs.sops.secrets = {
    gluetun_wireguard_private_key = {};
    gluetun_wireguard_addresses = {};
    transmission_password = {};
    slskd_username = {};
    slskd_password = {};
    immich_db_password = {};
    miniflux_db_password = {};
    miniflux_admin_password = {};
    miniflux_oidc_client_id = {};
    miniflux_oidc_client_secret = {};
    newt_id = {};
    newt_secret = {};
  };

  sops.templates = {
    "gluetun-env" = {
      content = ''
        WIREGUARD_PRIVATE_KEY=${config.sops.placeholder.gluetun_wireguard_private_key}
        WIREGUARD_ADDRESSES=${config.sops.placeholder.gluetun_wireguard_addresses}
      '';
      mode = "0400";
    };
    "transmission-env" = {
      content = ''
        TRANSMISSION_PASS=${config.sops.placeholder.transmission_password}
      '';
      mode = "0400";
    };
    "slskd-env" = {
      content = ''
        SLSKD_SLSK_USERNAME=${config.sops.placeholder.slskd_username}
        SLSKD_SLSK_PASSWORD=${config.sops.placeholder.slskd_password}
      '';
      mode = "0400";
    };
    "immich-db-env" = {
      content = ''
        DB_PASSWORD=${config.sops.placeholder.immich_db_password}
        POSTGRES_PASSWORD=${config.sops.placeholder.immich_db_password}
      '';
      mode = "0400";
    };
    "miniflux-db-env" = {
      content = ''
        POSTGRES_PASSWORD=${config.sops.placeholder.miniflux_db_password}
      '';
      mode = "0400";
    };
    "miniflux-admin-env" = {
      content = ''
        ADMIN_PASSWORD=${config.sops.placeholder.miniflux_admin_password}
      '';
      mode = "0400";
    };
    "miniflux-oidc-clientid-env" = {
      content = ''
        OAUTH2_CLIENT_ID=${config.sops.placeholder.miniflux_oidc_client_id}
      '';
      mode = "0400";
    };
    "miniflux-oidc-clientsecret-env" = {
      content = ''
        OAUTH2_CLIENT_SECRET=${config.sops.placeholder.miniflux_oidc_client_secret}
      '';
      mode = "0400";
    };
    "newt-env" = {
      content = ''
        NEWT_ID=${config.sops.placeholder.newt_id}
        NEWT_SECRET=${config.sops.placeholder.newt_secret}
      '';
      mode = "0400";
    };
  };

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
        secretsFile = config.sops.templates."gluetun-env".path;
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
        secrets.passwordFile = config.sops.templates."transmission-env".path;
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
        secretsFile = config.sops.templates."slskd-env".path;
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
        baseDir = "/data/apps/immich/files";
        dbDir = "/data/apps/immich/db";
        machineLearning = {
          enable = false;
          # Atlas delegates ML to vulcan instead of running a local sidecar.
          url = "http://vulcan.lan:3003";
        };
        secrets.databasePasswordFile = config.sops.templates."immich-db-env".path;
      };

      miniflux = {
        enable = true;
        baseDir = "/data/apps/miniflux/db";
        secrets = {
          databasePasswordFile = config.sops.templates."miniflux-db-env".path;
          adminPasswordFile = config.sops.templates."miniflux-admin-env".path;
          oidc = {
            enable = true;
            discoveryEndpoint = "https://auth.3679.space";
            redirectUrl = "https://rss.3679.space/oauth2/oidc/callback";
            providerName = "PocketID";
            userCreation = true;
            clientIdFile = config.sops.templates."miniflux-oidc-clientid-env".path;
            clientSecretFile = config.sops.templates."miniflux-oidc-clientsecret-env".path;
          };
        };
      };

      newt = {
        enable = true;
        pangolinEndpoint = "https://pangolin.3679.space";
        secretsFile = config.sops.templates."newt-env".path;
        dns = "192.168.0.2";
        # forgejo network will be added once forgejo is migrated.
        networks = ["default" "immich"];
      };
    };
  };
}
