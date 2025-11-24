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
    printers = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Printers available to the system.";
    };
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
      (mkIf (cfg.server) {
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
      (mkIf (cfg.client) {
        services.printing = {
          enable = true;
          drivers = with pkgs; [brlaser];
        };
        hardware.printers = {
          ensurePrinters = [
            {
              name = "Brother";
              location = "Home";
              deviceUri = "http://192.168.0.3:631/printers/Brother";
              model = "drv:///brlaser.drv/brl2320d.ppd";
              ppdOptions = {
                PageSize = "A4";
              };
            }
          ];
          ensureDefaultPrinter = "Brother";
        };
      })
    ]);
}
