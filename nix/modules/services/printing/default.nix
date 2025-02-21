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

  config = mkIf cfg.enable {
    services.avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
      publish = {
        enable = true;
        userServices = true;
      };
    };
    services.printing = {
      enable = true;
      listenAddresses = ["*:631"];
      allowFrom = ["all"];
      browsing = true;
      defaultShared = true;
      openFirewall = true;
      drivers = with pkgs; [brlaser brgenml1lpr brgenml1cupswrapper];
    };
    services.samba = {
      enable = true;
      package = pkgs.sambaFull;
      openFirewall = true;
      settings = {
        global = {
          "load printers" = "yes";
          "printing" = "cups";
          "printcap name" = "cups";
        };
        "printers" = {
          "comment" = "All Printers";
          "path" = "/var/spool/samba";
          "public" = "yes";
          "browseable" = "yes";
          # to allow user 'guest account' to print.
          "guest ok" = "yes";
          "writable" = "no";
          "printable" = "yes";
          "create mode" = 0700;
        };
      };
    };
    systemd.tmpfiles.rules = [
      "d /var/spool/samba 1777 root root -"
    ];

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
  };
}
