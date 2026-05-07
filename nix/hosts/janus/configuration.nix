{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  # Janus has two public IPs — services are split across them
  pangolinIp = "66.63.168.244";
  netbirdIp = "66.63.168.153";
in {
  imports = [
    ../../modules
    ./hardware.nix
    inputs.disko.nixosModules.disko
    ./disko.nix
  ];

  modules = {
    programs = {
      nushell = {
        enable = true;
        setDefault = true;
        zoxide.enable = true;
        carapace.enable = true;
      };
      sops = {
        enable = true;
        keyFile = null;
        secrets = {};
      };
    };
    services = {
      sudo-rs.enable = true;
      ssh = {
        enable = true;
        server = {
          enable = true;
          port = 6674;
        };
      };
      netbird.enable = true;
    };

    linux.oci = {
      enable = true;

      networks = {
        pangolin.enable = true;
        netbird.enable = true;
      };

      # Image versions/digests come from oci-images.json so an
      # auto-update workflow can rewrite plain JSON instead of nix.
      services = lib.recursiveUpdate (lib.importJSON ./oci-images.json) {
        pangolin = {
          enable = true;
          domain = "3679.space";
          dashboardDomain = "pangolin.3679.space";
          bindAddress = pangolinIp;
          baseDir = "/docker/config";
          adminEmail = "ryf@sent.as";
          email = {
            smtpHost = "smtp.fastmail.com";
            smtpPort = 465;
            smtpUser = "ryf@sent.as";
            noReply = "no-reply@3679.space";
          };
          openFirewall = true;
        };

        netbird = {
          enable = true;
          domain = "netbird.3679.space";
          authDomain = "auth.3679.space";
          bindAddress = netbirdIp;
          baseDir = "/docker/config/netbird";
          acmeEmail = "ryf@sent.as";
          openFirewall = true;
        };

        pocket-id = {
          enable = true;
          appUrl = "https://auth.3679.space";
          bindAddress = pangolinIp;
          baseDir = "/docker/config/pocket-id";
          networks = ["pangolin"];
        };
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
  };
}
