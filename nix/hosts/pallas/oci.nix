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
            dnsServerV6 = "2600:1702:6710:117F::FE";
          };
          # Both DNS hosts are statically addressed, so neither gets a
          # lease-derived record; must match hecate's list for failover.
          dns.hostRecords = [
            "pallas.lan,192.168.0.2"
            "hecate.lan,192.168.0.77"
          ];
        };
        caddy = {
          enable = true;
          baseDir = "/docker/caddy";
          reverseProxies = import ./reverse-proxies.nix;
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
