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
            publish = {
              enable = true;
              userServices = true;
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
          };
        };

        # The generated ensure-printers unit only orders after cups.service, but a
        # client queue has a network deviceUri: at boot lpadmin can run before the
        # NIC has carrier, and one EHOSTDOWN leaves the host with no queue at all
        # (oneshot, no retry — systemd forbids Restart= with RemainAfterExit=yes).
        # network-online.target is passive, so it must be pulled in, not just
        # ordered against.
        systemd.services.ensure-printers = {
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
