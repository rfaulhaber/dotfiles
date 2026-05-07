{
  config,
  lib,
  ...
}: {
  modules.linux.oci = {
    enable = true;
    services =
      lib.importJSON ./oci-images.json
      |> lib.recursiveUpdate {
        pihole = {
          enable = true;
          baseDir = "/docker/pihole";
          interface = "end0";
          webPasswordFile = config.sops.templates."pihole-env".path;
          dhcp = {
            enable = true;
            start = "192.168.0.3";
            end = "192.168.0.253";
            router = "192.168.0.1";
            ipv6 = true;
            rapidCommit = true;
            dnsServer = "192.168.0.254";
          };
        };
        caddy = {
          enable = true;
          baseDir = "/docker/caddy";
          reverseProxies = {
            radarr = {
              hosts = ["radarr.home.lan"];
              upstream = "192.168.0.3:7878";
            };
            sonarr = {
              hosts = ["sonarr.home.lan"];
              upstream = "192.168.0.3:8989";
            };
            lidarr = {
              hosts = ["lidarr.home.lan"];
              upstream = "192.168.0.3:8686";
            };
            slskd = {
              hosts = ["slskd.home.lan"];
              upstream = "192.168.0.3:5030";
            };
            calibre-web = {
              hosts = ["calibre-web.home.lan"];
              upstream = "192.168.0.3:8089";
              displayName = "Calibre-Web";
            };
            jellyfin = {
              hosts = ["jellyfin.home.lan"];
              upstream = "192.168.0.105:8096";
              displayName = "Jellyfin";
            };
            plex = {
              hosts = ["plex.home.lan"];
              upstream = "192.168.0.105:32400";
              displayName = "Plex";
            };
            music = {
              hosts = ["music.home.lan"];
              upstream = "192.168.0.3:4533";
            };
            prowlarr = {
              hosts = ["prowlarr.home.lan"];
              upstream = "192.168.0.3:9696";
              displayName = "Prowlarr";
            };
            transmission = {
              hosts = ["transmission.home.lan"];
              upstream = "192.168.0.3:9091";
              displayName = "Transmission";
            };
            nzbget = {
              hosts = ["nzbget.home.lan"];
              upstream = "192.168.0.3:6789";
              displayName = "Nzbget";
            };
            requestrr = {
              hosts = ["requestrr.home.lan"];
              upstream = "192.168.0.3:4545";
            };
            bazarr = {
              hosts = ["bazarr.home.lan"];
              upstream = "192.168.0.3:6767";
            };
            pihole = {
              hosts = ["pihole.home.lan"];
              upstream = "192.168.0.2:8085";
              displayName = "Pi-hole";
            };
            tautulli = {
              hosts = ["tautulli.home.lan"];
              upstream = "192.168.0.3:8181";
              displayName = "Tautulli";
            };
            git = {
              hosts = ["git.home.lan"];
              upstream = "192.168.0.3:2835";
            };
            photos = {
              hosts = ["photos.home.lan"];
              upstream = "192.168.0.3:2283";
            };
          };
          index = {
            enable = true;
            hosts = ["home.lan"];
            title = "Service Index";
            description = "Available services on the local network";
          };
        };
      };
  };
}
