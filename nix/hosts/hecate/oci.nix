{
  config,
  lib,
  ...
}: {
  modules.linux.oci = {
    enable = true;
    # Image versions/digests come from oci-images.json so an
    # auto-update workflow can rewrite plain JSON instead of nix.
    services =
      lib.importJSON ./oci-images.json
      |> lib.recursiveUpdate {
        pihole = {
          enable = true;
          baseDir = "/docker/pihole";
          interface = "enu1u1u1";
          webPasswordFile = config.sops.templates."pihole-env".path;
          # DHCP disabled — hecate is a backup DNS only
          # Both DNS hosts are statically addressed, so neither gets a
          # lease-derived record; must match pallas's list for failover.
          dns.hostRecords = [
            "pallas.lan,192.168.0.2"
            "hecate.lan,192.168.0.77"
          ];
        };
      };
  };
}
