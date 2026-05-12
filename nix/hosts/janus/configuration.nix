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
