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

    linux.oci = {
      enable = true;
      # Image versions/digests come from oci-images.json so an
      # auto-update workflow can rewrite plain JSON instead of nix.
      services = lib.recursiveUpdate (lib.importJSON ./oci-images.json) {
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
          };
        };
        caddy = {
          enable = true;
          baseDir = "/docker/caddy";
          reverseProxies = {
            radarr = {
              hosts = ["radarr.home.lan"];
              upstream = "192.168.0.3:7878";
            };
            sonarr = {
              hosts = ["sonarr.home.lan"];
              upstream = "192.168.0.3:8989";
            };
            lidarr = {
              hosts = ["lidarr.home.lan"];
              upstream = "192.168.0.3:8686";
            };
            slskd = {
              hosts = ["slskd.home.lan"];
              upstream = "192.168.0.3:5030";
            };
            calibre-web = {
              hosts = ["calibre-web.home.lan"];
              upstream = "192.168.0.3:8089";
              displayName = "Calibre-Web";
            };
            jellyfin = {
              hosts = ["jellyfin.home.lan"];
              upstream = "192.168.0.105:8096";
              displayName = "Jellyfin";
            };
            plex = {
              hosts = ["plex.home.lan"];
              upstream = "192.168.0.105:32400";
              displayName = "Plex";
            };
            music = {
              hosts = ["music.home.lan"];
              upstream = "192.168.0.3:4533";
            };
            prowlarr = {
              hosts = ["prowlarr.home.lan"];
              upstream = "192.168.0.3:9696";
              displayName = "Prowlarr";
            };
            transmission = {
              hosts = ["transmission.home.lan"];
              upstream = "192.168.0.3:9091";
              displayName = "Transmission";
            };
            nzbget = {
              hosts = ["nzbget.home.lan"];
              upstream = "192.168.0.3:6789";
              displayName = "Nzbget";
            };
            requestrr = {
              hosts = ["requestrr.home.lan"];
              upstream = "192.168.0.3:4545";
            };
            bazarr = {
              hosts = ["bazarr.home.lan"];
              upstream = "192.168.0.3:6767";
            };
            pihole = {
              hosts = ["pihole.home.lan"];
              upstream = "192.168.0.2:8085";
              displayName = "Pi-hole";
            };
            tautulli = {
              hosts = ["tautulli.home.lan"];
              upstream = "192.168.0.3:8181";
              displayName = "Tautulli";
            };
            git = {
              hosts = ["git.home.lan"];
              upstream = "192.168.0.3:2835";
            };
            photos = {
              hosts = ["photos.home.lan"];
              upstream = "192.168.0.3:2283";
            };
          };
          index = {
            enable = true;
            hosts = ["home.lan"];
            title = "Service Index";
            description = "Available services on the local network";
          };
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
