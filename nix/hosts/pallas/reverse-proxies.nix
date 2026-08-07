let
  base = "192.168.0.";
  pallas = "${base}2";
  atlas = "${base}3";
  vulcan = "${base}105";
in {
  radarr = {
    hosts = ["radarr.home.lan"];
    upstream = "${atlas}:7878";
  };
  sonarr = {
    hosts = ["sonarr.home.lan"];
    upstream = "${atlas}:8989";
  };
  lidarr = {
    hosts = ["lidarr.home.lan"];
    upstream = "${atlas}:8686";
  };
  slskd = {
    hosts = ["slskd.home.lan"];
    upstream = "${atlas}:5030";
  };
  calibre-web = {
    hosts = ["calibre-web.home.lan"];
    upstream = "${atlas}:8089";
    displayName = "Calibre-Web";
  };
  jellyfin = {
    hosts = ["jellyfin.home.lan"];
    upstream = "${vulcan}:8096";
    displayName = "Jellyfin";
  };
  plex = {
    hosts = ["plex.home.lan"];
    upstream = "${vulcan}:32400";
    displayName = "Plex";
  };
  music = {
    hosts = ["music.home.lan"];
    upstream = "${atlas}:4533";
  };
  prowlarr = {
    hosts = ["prowlarr.home.lan"];
    upstream = "${atlas}:9696";
    displayName = "Prowlarr";
  };
  transmission = {
    hosts = ["transmission.home.lan"];
    upstream = "${atlas}:9091";
    displayName = "Transmission";
  };
  nzbget = {
    hosts = ["nzbget.home.lan"];
    upstream = "${atlas}:6789";
    displayName = "Nzbget";
  };
  requestrr = {
    hosts = ["requestrr.home.lan"];
    upstream = "${atlas}:4545";
  };
  bazarr = {
    hosts = ["bazarr.home.lan"];
    upstream = "${atlas}:6767";
  };
  pihole = {
    hosts = ["pihole.home.lan"];
    upstream = "${pallas}:8085";
    displayName = "Pi-hole";
  };
  tautulli = {
    hosts = ["tautulli.home.lan"];
    upstream = "${atlas}:8181";
    displayName = "Tautulli";
  };
  git = {
    hosts = ["git.home.lan"];
    upstream = "${atlas}:2835";
  };
  photos = {
    hosts = ["photos.home.lan"];
    upstream = "${atlas}:2283";
  };
  grafana = {
    hosts = ["grafana.home.lan"];
    upstream = "${atlas}:3000";
    # OIDC session cookie + auth code shouldn't cross the LAN cleartext.
    # caddy auto-redirects http://grafana.home.lan here. NOTE: TLS
    # terminates on pallas — the hop to atlas:3000 is still plain http.
    scheme = "https";
    tls = "internal";
  };
  prometheus = {
    hosts = ["prometheus.home.lan"];
    upstream = "${atlas}:9090";
  };
  open-webui = {
    hosts = ["webui.home.lan"];
    upstream = "${vulcan}:3000";
    displayName = "Open WebUI";
  };
  calibre = {
    hosts = ["calibre.home.lan"];
    upstream = "${atlas}:3229";
    displayName = "Calibre";
    scheme = "https";
    tls = "internal";
    # The Selkies build only serves the desktop UI over https (container
    # 8181); its self-signed cert can't match the IP caddy dials.
    upstreamScheme = "https";
    upstreamTlsInsecure = true;
  };
}
