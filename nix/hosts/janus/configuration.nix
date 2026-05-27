{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib.importJSON ./ips.json) pangolin netbird;
  pangolinIp = pangolin;
  netbirdIp = netbird;
in {
  imports = [
    ../../modules
    ./oci.nix
    ./hardware.nix
    inputs.disko.nixosModules.disko
    ./disko.nix
  ];

  modules = {
    programs = {
      btop.enable = true;
      nushell = {
        enable = true;
        setDefault = true;
        zoxide.enable = true;
        carapace.enable = true;
      };
      sops = {
        enable = true;
        keyFile = null;
        secrets = {
          "netbird-client/setup-key" = {};
        };
      };
    };
    services = {
      sudo-rs.enable = true;
      observability-agent = {
        enable = true;
        loki = {
          # atlas.lan doesn't resolve on a cloud VPS — reach atlas's loki
          # over the netbird overlay instead.
          url = "http://atlas.netbird.selfhosted:3100";
          extraLabels.role = "cloud-vps";
        };
        # Leave prometheus.openFirewall = false (default); 9100 is narrowed
        # to the wt0 interface below so node_exporter isn't exposed to the
        # public internet on janus's ens3.
      };
      ssh = {
        enable = true;
        server = {
          enable = true;
          port = 6674;
        };
      };
      netbird = {
        enable = true;
        setupKeyFile = config.sops.secrets."netbird-client/setup-key".path;
      };
    };

    themes.active = "tokyo-night-dark";
  };

  boot = {
    loader.grub = {
      enable = true;
      efiSupport = true;
      efiInstallAsRemovable = true;
    };
  };

  networking = {
    hostName = "janus";
    hostId = "66a2b43a";

    interfaces.ens3 = {
      useDHCP = true;
      ipv4.addresses = [
        {
          address = netbirdIp;
          prefixLength = 24;
        }
        {
          address = pangolinIp;
          prefixLength = 24;
        }
      ];
    };

    # Expose the metrics exporters only over the netbird overlay; ens3 is
    # public, so we deliberately don't toggle `openFirewall` on the agent.
    firewall.interfaces.wt0.allowedTCPPorts = [
      9100 # node_exporter
      9882 # podman-exporter
    ];
  };
}
