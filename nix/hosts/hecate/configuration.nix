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
          port = 17263;
        };
      };
      netbird.enable = true;
      keepalived = {
        enable = true;
        interface = "enu1u1u1";
        state = "BACKUP";
        priority = 90;
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

    interfaces.enu1u1u1 = {
      ipv6.addresses = let
        addresses = [
          "2600:1702:6710:117f:3926:9b5e:e4ac:7e55"
          "2600:1702:6710:117f:ba27:ebff:fe45:71fc"
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
      allowedTCPPorts = [8085 80 53 443];
      allowedUDPPorts = [53 123];
    };
  };

  # temporary, make nix settings modular
  nix.gc.automatic = lib.mkForce false;
}
