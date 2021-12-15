# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ../../modules
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  modules = {
    programs = {
      zsh = {
        enable = true;
        setDefault = true;
      };
      neovim.enable = true;
      git.enable = true;
    };
    services = {
      docker.enable = true;
      gpg.enable = true;
      virt.enable = true;
      systemd.modules = [ "updatedb" ];
      ssh = {
        enable = true;
        enableServer = true;
      };
      snapraid = {
        enable = true;
        parityFiles = [ "/diskp/snapraid.parity" ];
        dataDisks = {
          disk1 = "/disk1/";
          disk2 = "/disk2/";
          disk3 = "/disk3/";
          disk4 = "/disk4/";
        };
        contentFiles =
          [ "/var/snapraid/snapraid.content" "/diskp/snapraid.content" ];
      };
      mergerfs = {
        enable = true;
        branches = [ "/disk1" "/disk2" "/disk3" ];
        target = "/data";
      };
      calibre-serve = {
        enable = true;
        sharePoint = /data/calibre;
      };
    };
    themes.active = "city-lights";
  };

  nixpkgs.config.allowUnfree = true;

  boot = {
    tmpOnTmpfs = true;
    cleanTmpDir = true;

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      grub.device = "nodev";
    };
  };

  networking = {
    hostName = "atlas";

    useDHCP = false;
    interfaces.eno1.useDHCP = true;
    interfaces.eno2.useDHCP = true;

    # TODO move
    firewall = {
      enable = true;
      allowedTCPPorts = [ 32400 ];
    };
  };

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.ryan = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJHGYKXsTAHFyfjUzynjFd1XcfnL3tO90v/2BzSBq3yK ryf@sent.as"
    ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [ snapraid mergerfs ];

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

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
    autoUpgrade = {
      enable = true;
      allowReboot = true;
    };
    stateVersion = "21.11";
  };

}

