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
    inputs.nixos-hardware.nixosModules.raspberry-pi-3
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
        prometheus = {
          openFirewall = true;
          interface = "enu1u1u1";
        };
        loki.extraLabels.role = "dns-backup";
      };
      ssh = {
        enable = true;
        server = {
          enable = true;
          port = 17263;
        };
      };
      netbird = {
        enable = true;
        setupKeyFile = config.sops.secrets."netbird/setup-key".path;
        # hecate is the backup Pi-hole resolver. Leave resolv.conf under NixOS
        # control so the declared non-Pi-hole fallback below survives Pi-hole
        # downtime; otherwise Netbird points the host at its own embedded
        # resolver (which depends on Pi-hole) and an image-bump deploy
        # deadlocks: stopping the old container kills DNS, and the new
        # container's image pull then can't resolve the registry.
        manageDNS = false;
      };
      keepalived = {
        enable = true;
        interface = "enu1u1u1";
        state = "BACKUP";
        priority = 90;
        virtualIps = ["192.168.0.254/24"];
        virtualIpv6s = ["fe80::FE/64" "2600:1702:6710:117F::FE/64"];
        # Deliberately in-repo: VRRPv2 PASS auth travels cleartext in every
        # advert, so this is a peer-sync token, not a secret. Must match
        # pallas's.
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

  services.timesyncd.servers = ntpServers;

  networking = {
    hostName = "hecate";
    useDHCP = true;
    timeServers = ntpServers;

    # Local Pi-hole first; the public resolvers are the failover path the host
    # falls through to (connection-refused on a stopped Pi-hole) so registry
    # and NTP-by-name lookups keep working while the container is down. Three
    # static entries fill glibc's MAXNS quota, so a DHCP-provided resolver
    # (the keepalived VIP, which can resolve back to this host's own Pi-hole
    # when hecate holds it) never displaces the fallback. Only effective
    # because Netbird DNS management is disabled above.
    nameservers = ["127.0.0.1" "1.1.1.1" "1.0.0.1"];
    # openresolv defaults resolv_conf_local_only=YES: when a loopback resolver
    # is in the list it writes ONLY 127.0.0.1 to resolv.conf and silently drops
    # the public fallback above — reinstating the very deadlock this is meant to
    # prevent. Force it to emit every nameserver so glibc can fail over to the
    # public resolvers when the local Pi-hole container is down.
    resolvconf.extraConfig = "resolv_conf_local_only=NO\n";

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
}
