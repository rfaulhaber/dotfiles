{config, ...}: {
  modules.linux.oci = {
    enable = true;
    services = {
      pihole = {
        enable = true;
        baseDir = "/docker/pihole";
        interface = "enu1u1u1";
        webPasswordFile = config.sops.templates."pihole-env".path;
        # DHCP disabled — hecate is a backup DNS only
      };
    };
  };
}
