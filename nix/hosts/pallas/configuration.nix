{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  # Pin NTP to literal IPs so time sync never depends on DNS. This RTC-less node
  # otherwise deadlocks on cold boot: a stale clock makes DNSSEC validation
  # SERVFAIL, but correcting the clock needs NTP, which can't resolve its pool
  # hostnames while DNS (Pi-hole) is down. networking.timeServers only feeds
  # timesyncd's FallbackNTP; set servers too so the primary NTP= line is IPs as
  # well, leaving no DNS-dependent path on either list.
  ntpServers = [
    "162.159.200.1" # time.cloudflare.com
    "162.159.200.123" # time.cloudflare.com (secondary)
    "216.239.35.0" # time.google.com
    "216.239.35.4" # time.google.com
  ];
in {
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
      btop.enable = true;
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
          "netbird/setup-key" = {};
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
      netbird = {
        enable = true;
        setupKeyFile = config.sops.secrets."netbird/setup-key".path;
        # pallas IS the network's Pi-hole resolver. Leave resolv.conf under
        # NixOS control so the declared non-Pi-hole fallback below survives
        # Pi-hole downtime; otherwise Netbird points the host at its own
        # embedded resolver (which depends on Pi-hole) and an image-bump deploy
        # deadlocks: stopping the old container kills DNS, and the new
        # container's image pull then can't resolve the registry.
        manageDNS = false;
      };
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

  services.timesyncd.servers = ntpServers;

  networking = {
    hostName = "pallas";
    useDHCP = false;
    defaultGateway = "192.168.0.1";
    # Local Pi-hole first; the public resolvers are the failover path the host
    # falls through to (connection-refused on a stopped Pi-hole) so registry
    # and NTP-by-name lookups keep working while the container is down. Only
    # effective because Netbird DNS management is disabled above — otherwise it
    # would overwrite this list. glibc honours at most the first three.
    nameservers = ["127.0.0.1" "1.1.1.1" "1.0.0.1"];
    # openresolv defaults resolv_conf_local_only=YES: when a loopback resolver
    # is in the list it writes ONLY 127.0.0.1 to resolv.conf and silently drops
    # the public fallback above — reinstating the very deadlock this is meant to
    # prevent. Force it to emit every nameserver so glibc can fail over to the
    # public resolvers when the local Pi-hole container is down.
    resolvconf.extraConfig = "resolv_conf_local_only=NO\n";
    timeServers = ntpServers;

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
}
