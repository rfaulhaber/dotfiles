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
    ./oci.nix
    inputs.nixos-hardware.nixosModules.raspberry-pi-4
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
      observability-agent = {
        enable = true;
        prometheus.openFirewall = true;
        loki.extraLabels.role = "dns-primary";
      };
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
        interface = "end0";
        state = "MASTER";
        priority = 100;
        virtualIps = ["192.168.0.254/24"];
        virtualIpv6s = ["fe80::FE/64" "2600:1702:6710:117F::FE/64"];
        authPass = "0x7gMrmq";
        healthCheck.enable = true;
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
    kernelPackages = pkgs.linuxPackages;
    initrd.availableKernelModules = ["xhci_pci" "usbhid" "usb_storage"];
    loader = {
      grub.enable = false;
      generic-extlinux-compatible.enable = true;
    };
  };

  hardware.enableRedistributableFirmware = true;

  console.enable = false;

  environment.systemPackages = with pkgs; [
    libraspberrypi
    raspberrypi-eeprom
  ];

  networking = {
    hostName = "pallas";
    useDHCP = false;
    defaultGateway = "192.168.0.1";
    nameservers = ["127.0.0.1" "1.1.1.1"];

    defaultGateway6 = {
      address = "2600:1702:6710:117F:56AF:97FF:FE12:496C";
      interface = "end0";
    };

    interfaces.end0 = {
      ipv4.addresses = [
        {
          address = "192.168.0.2";
          prefixLength = 24;
        }
      ];
      ipv6.addresses = let
        addresses = [
          "2600:1702:6710:117F:C40A:AFB1:A677:52E4"
          "2600:1702:6710:117F:487B:3A4C:4399:5458"
          "2600:1702:6710:117F:DA3A:DDFF:FEDA:2B5"
        ];
      in
        builtins.map (address: {
          inherit address;
          prefixLength = 64;
        })
        addresses;
    };

    firewall = {
      enable = true;
      # required for pihole
      allowedTCPPorts = [8085 80 53 67 443];
      allowedUDPPorts = [53 67 68 123 546 547];
      extraCommands = ''
        iptables -I INPUT 1 -p tcp -m tcp --dport 4711 -i lo -j ACCEPT
        iptables -I INPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
      '';
    };
  };

  # temporary, make nix settings modular
  nix.gc.automatic = lib.mkForce false;
}
