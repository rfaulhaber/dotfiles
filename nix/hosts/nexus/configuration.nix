# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    # ./hardware-configuration.nix
    ../../modules
    inputs.nixos-generators.nixosModules.all-formats
  ];

  modules = {
    programs = {
      nushell = {
        enable = true;
        setDefault = true;
        carapace.enable = true;
      };
      neovim.enable = true;
      git.enable = true;
    };
    services = {
      ssh = {
        enable = true;
        server = {
          enable = true;
          keys = [
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHlCcs8h2PrT3GcOVs6K0IGozqV8yuR945ZDr8eYhqfj ryan@hyperion"
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAID9EVRAxaCrK68NSCoiNjjQLqu4k13Z45tCBb0jGAtC/ ryan@eos"
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPhN+t0aI3pQhZsFPoRn8dWe7YvDn3ehWOUmwvqbQyZP ryan@1p"
          ];
          port = 14557;
        };
      };

      # zerotier = {
      #   enable = true;
      #   networks = ["b6079f73c6986bc2"];
      # };
    };

    themes.active = "tokyo-night-dark";
  };

  virtualisation.vmVariant = {
    virtualisation.forwardPorts = [
      {
        from = "host";
        host.port = 14557;
        guest.port = 14557;
      }
    ];
  };


  boot = {
    loader = {
      grub.enable = false;
      generic-extlinux-compatible.enable = true;
    };
  };

  networking = {
    hostName = "nexus";
    useDHCP = true;
    interfaces.eth0.useDHCP = true;
  };

  user.initialHashedPassword = "$2b$05$DHs2w8s5PkPPDbbjeNWWSO.98pOyBVSdv2b.eP1M.MXOM5eEkmGAS";
}
