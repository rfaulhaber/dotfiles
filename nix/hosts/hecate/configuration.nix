{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    ../../modules
    inputs.nixos-hardware.nixosModules.raspberry-pi-3
    inputs.determinate.nixosModules.default
  ];

  nix.settings = {
    substituters = ["https://install.determinate.systems"];
    trusted-public-keys = ["cache.flakehub.com-3:hJuILl5sVK4iKm86JzgdXW12Y2Hwd5G07qKtHTOcDCM="];
  };

  modules = {
    programs = {
      nushell = {
        enable = true;
        setDefault = true;
        carapace.enable = true;
      };
      sops = {
        enable = true;
        keyFile = null;
        secrets = {
          pihole-web-password = {};
        };
      };
    };
    services = {
      sudo-rs.enable = true;
      ssh = {
        enable = true;
        server = {
          enable = true;
          port = 12981;
        };
      };
      netbird.enable = true;
      keepalived = {
        enable = true;
        interface = "eth0"; # TODO: verify interface name on the Pi 3B
        state = "BACKUP";
        priority = 90;
        virtualIps = ["192.168.0.254/24"];
        authPass = "0x7gMrmq";
        healthCheck.enable = true;
      };
    };

    linux.oci = {
      enable = true;
      services = {
        pihole = {
          enable = true;
          baseDir = "/docker/pihole";
          interface = "eth0"; # TODO: verify interface name on the Pi 3B
          webPasswordFile = config.sops.templates."pihole-env".path;
          # DHCP disabled — hecate is a backup DNS only
        };
      };
    };

    themes.active = "moonlight";
  };

  # sops template for pihole env file (KEY=VALUE format for environmentFiles)
  sops.templates."pihole-env" = {
    content = "FTLCONF_webserver_api_password=${config.sops.placeholder."pihole-web-password"}";
    owner = "root";
  };

  boot = {
    # The RPi Foundation kernel supports Pi 3 despite the "rpi4" name
    kernelPackages = pkgs.linuxKernel.packages.linux_rpi4;
    initrd.availableKernelModules = ["usbhid" "usb_storage"];
    loader = {
      grub.enable = false;
      generic-extlinux-compatible.enable = true;
    };
  };

  hardware.enableRedistributableFirmware = true;

  console.enable = false;

  environment.systemPackages = with pkgs; [
    libraspberrypi
  ];

  networking = {
    hostName = "hecate";
    useDHCP = true;

    # TODO: configure static IPv6 addresses once the host is provisioned
    # interfaces.eth0 = { ... };

    firewall = {
      enable = true;
      # required for pihole
      allowedTCPPorts = [8085 80 53 443];
      allowedUDPPorts = [53 123];
    };
  };

  # temporary, make nix settings modular
  nix.gc.automatic = lib.mkForce false;
}
