# a lot of the configuration here is hard-coded because it suits my specific needs.
# hypothetically, I could make it more modular, but I so rarely need to print anything as it is
# that I probably won't.
# TODO consider making this a container of some sort
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.printing;
in {
  options.modules.services.printing = {
    enable = mkEnableOption false;
    server = mkOption {
      type = types.bool;
      description = "If enabled, enables print server.";
      default = false;
    };
    client = mkOption {
      type = types.bool;
      description = "If enabled, configures system to connect to print server.";
      default = false;
    };
  };

  config =
    mkIf cfg.enable
    (mkMerge [
      (mkIf cfg.server {
        services = {
          avahi = {
            enable = true;
            nssmdns4 = true;
            openFirewall = true;
            # The LAN routers advertise the v6 prefix with 300s valid=preferred
            # lifetimes, so the SLAAC address expires and is deleted whenever an
            # RA runs a few seconds late (~every 8 minutes). Each flap makes
            # avahi withdraw and re-probe its AAAA record, and it occasionally
            # loses the probe race against its own looped-back packets — renaming
            # the host (atlas -> atlas-N). cupsd bakes the hostname into the
            # printer's SRV target when it registers and never follows a rename,
            # so a single rename leaves clients with a target that resolves to
            # nothing. Discovery works fine over v4 alone.
            settings = {
              server.use-ipv6 = false;
              publish = {
                disable-publishing = false;
                # cupsd registers the printer as a user service, and clients
                # resolve its SRV target through this host's address records.
                disable-user-service-publishing = false;
                publish-addresses = true;
                # use-ipv6=no only closes the v6 socket: publish-aaaa-on-ipv4
                # defaults to yes, so avahi still tracks every v6 address and
                # publishes its AAAA record over v4, and the flap-driven renames
                # continue. Dropping AAAA publication is what removes v6 from
                # avahi's view entirely.
                publish-aaaa-on-ipv4 = false;
              };
            };
          };
          printing = {
            enable = true;
            listenAddresses = ["*:631"];
            allowFrom = [
              "all"
            ];
            browsing = true;
            defaultShared = true;
            openFirewall = true;
            drivers = with pkgs; [brlaser];
          };
        };

        hardware.printers = {
          ensurePrinters = [
            {
              name = "Brother";
              location = "Home";
              deviceUri = "usb://Brother/HL-L2320D%20series?serial=U63877F3N351724";
              model = "drv:///brlaser.drv/brl2320d.ppd";
              ppdOptions = {
                PageSize = "A4";
              };
            }
          ];
          ensureDefaultPrinter = "Brother";
        };
      })
      (mkIf cfg.client {
        services = {
          printing.enable = true;
          avahi = {
            enable = true;
            nssmdns4 = true;
            openFirewall = true;
            # Same v6 RA-lifetime flap as the server block; browsing needs v4 only.
            settings = {
              server.use-ipv6 = false;
              publish.publish-aaaa-on-ipv4 = false;
            };
          };
        };

        # hardware.printers provisions queues from cups.service's ExecStartPost under
        # `set -e`, and an `everywhere` queue makes lpadmin query the server. At boot
        # that runs before the NIC has carrier, so EHOSTDOWN fails the unit and
        # Restart=on-failure burns through the start limit within two seconds,
        # leaving cupsd and cups.socket dead until a manual restart.
        # network-online.target is passive, so it must be pulled in, not just
        # ordered against.
        systemd.services.cups = {
          wants = ["network-online.target"];
          after = ["network-online.target"];
        };

        # Deliberately driverless: a real PPD here makes the client rasterize to
        # application/vnd.cups-raster before submitting, and the server's own
        # filter chain then fails on the already-converted stream. Filtering has
        # to happen exactly once, on the host that owns the hardware.
        hardware.printers = {
          ensurePrinters = [
            {
              name = "Brother";
              location = "Home";
              deviceUri = "ipp://192.168.0.3:631/printers/Brother";
              model = "everywhere";
            }
          ];
          ensureDefaultPrinter = "Brother";
        };
      })
    ]);
}
