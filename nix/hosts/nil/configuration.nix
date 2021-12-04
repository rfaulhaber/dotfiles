# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ../../modules
    ./hardware-configuration.nix
  ];

  modules = {
    programs = {
      zsh = {
        enable = true;
        setDefault = true;
      };
      git.enable = true;
    };
    services = {
      docker.enable = true;
      ssh = {
        enable = true;
        keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJHGYKXsTAHFyfjUzynjFd1XcfnL3tO90v/2BzSBq3yK ryf@sent.as"
        ];
      };
    };
  };

  networking = {
    hostName = "nil";
    useDHCP = false;
    interfaces = { eth0 = { useDHCP = true; }; };
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.tmpOnTmpfs = true;
  boot.cleanTmpDir = true;

  nix = {
    autoOptimiseStore = true;
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

  system = {
    autoUpgrade = { enable = true; };
    stateVersion = "21.11";
  };

}

