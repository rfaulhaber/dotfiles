# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

let
  unstable = import <unstable> { config = config.nixpkgs.config; };
  pcloud = import ./pcloud.nix pkgs;
in {
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    # <home-manager/nixos>
  ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.pulseaudio = true;

  boot.tmpOnTmpfs = true;
  boot.cleanTmpDir = true;
  boot.loader = {
    systemd-boot.enable = true;
    grub = {
      enable = true;
      version = 2;
      useOSProber = true;
      efiSupport = true;
      device = "nodev";
    };

    efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint = "/boot";
    };
  };

  networking = {
    hostName = "mir3";
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;

    interfaces = { enp5s0 = { useDHCP = true; }; };

    networkmanager.enable = true;
  };

  # Set your time zone.
  time = {
    timeZone = "America/New_York";
    hardwareClockInLocalTime = true;
  };

  location.provider = "geoclue2";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # TODO clean up!
  environment.systemPackages = with pkgs; [
    oh-my-zsh
    zsh
    zsh-autosuggestions
    zsh-completions

    #desktop
    betterlockscreen
    bspwm
    chromium
    unstable.discord
    evince
    feh
    firefox-devedition-bin
    gnome3.gnome-screenshot
    gnome3.gnome-bluetooth
    unstable.keybase
    unstable.keybase-gui
    unstable.kbfs
    unstable.kitty
    keychain
    openvpn
    pass
    # too out of date, will replace once stable
    # unstable.pcloud
    pcloud
    polybarFull
    pulsemixer
    pavucontrol
    redshift
    unstable.rofi
    spotify
    sxhkd
    tdesktop
    xclip
    xscreensaver
    xtitle
    unstable.ripcord
    unstable.wally-cli

    #dev

    #dev.util
    coreutils-full
    docker
    docker-compose
    docker-machine
    git
    rsync
    shellcheck
    shfmt
    tokei
    unstable.gnumake
    unstable.just

    #dev.rust
    rustup

    #dev.racket
    unstable.racket

    #dev.editors
    neovim
    unstable.emacs

    nixfmt
    direnv

    #util
    bat
    cifs-utils
    croc
    curl
    exa
    fd
    fzf
    gnupg
    htop
    jq
    pandoc
    smbclient
    stow
    texlive.combined.scheme-medium
    unstable.kvm
    unstable.qemu
    unstable.qemu-utils
    unstable.ripgrep
    unstable.zoxide
    unzip
    wget
    zip

    #emacs
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    cmake
    djvulibre
    graphviz
    imagemagick
    libtool # vterm
    libvterm
    sqlite
    unstable.isync
    unstable.mu
    unstable.nodePackages.mermaid-cli
    wordnet
    xdotool
    xorg.xwininfo

    #system deps
    python3
    libsecret
  ];

  environment.etc."xdg/mimeapps.list" = {
    text = ''
      [Default Applications]
      application/pdf=org.gnome.Evince.desktop;emacs.desktop
    '';
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "gnome3";
  };

  programs.evince.enable = true;

  programs.seahorse.enable = true;

  programs.zsh = {
    enable = true;
    ohMyZsh = {
      enable = true;
      plugins = [ "git" "colored-man-pages" ];
      theme = "agnoster";
    };
    autosuggestions.enable = true;
    autosuggestions.highlightStyle = "fg=#41505E";
    syntaxHighlighting.enable = true;
    shellAliases = {
      pbcopy = "xclip -selection clipboard";
      pbpaste = "xclip -selection clipboard -o";
      vi = "nvim";
      vim = "nvim";
      ls = "exa";
      l = "exa -lah";
      ll = "exa -lh";
      ec = "emacsclient";
      eo = "emacsclient -n"; # "emacs open"
    };
  };

  environment.variables = rec {
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_BIN_HOME = "$HOME/.local/bin";
    RUSTUP_HOME = "${XDG_DATA_HOME}/rustup";
    CARGO_HOME = "${XDG_DATA_HOME}/cargo";
    ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE = "fg=#41505E";
  };

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    enableDefaultFonts = true;
    fonts = with pkgs; [
      (unstable.nerdfonts.override { fonts = [ "Hack" ]; })
      lato
      merriweather
    ];
    fontconfig.defaultFonts = {
      serif = [ "Merriweather" ];
      sansSerif = [ "Lato" ];
      monospace = [ "Hack Nerd Font Mono" ];
    };
  };

  i18n = { defaultLocale = "en_US.UTF-8"; };

  # List services that you want to enable:

  services = {
    printing.enable = true;
    xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "eurosign:e";
      windowManager = { bspwm = { enable = true; }; };
      displayManager = {
        lightdm.enable = true;
        defaultSession = "none+bspwm";
        sessionCommands = ''
          ~/Projects/dotfiles/nix/hosts/mir3/random-wallpaper.sh
        '';
      };
      videoDrivers = [ "nvidia" ];
    };

    # it is unclear to me how to automatically unlock gnome keyring upon login, so
    # I'm taking the shotgun approach
    gnome = { gnome-keyring.enable = true; };
    dbus.packages = with pkgs; [ gnome.gnome-keyring gcr gnome3.dconf ];

    emacs = {
      enable = true;
      install = true;
      defaultEditor = true;
      package = unstable.emacs;
    };

    redshift = {
      enable = true;
      brightness = {
        day = "1";
        night = "1";
      };
      temperature = {
        day = 5700;
        night = 2600;
      };
    };

    keybase.enable = true;
    kbfs.enable = true;
    udev = {
      extraRules = ''
        SUBSYSTEM=="usb", ATTR{idVendor}=="3297", ATTR{idProduct}=="1969", GROUP="plugdev"
      '';
    };

    blueman.enable = true;
  };

  virtualisation.docker.enable = true;
  security.pam.services.lightdm.enableGnomeKeyring = true;

  # Enable sound.

  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    package = pkgs.pulseaudioFull;
  };

  hardware.bluetooth = {
    enable = true;
    settings.General.Enable = "Source,Sink,Media,Socket";
  };

  users.groups = { plugdev = { }; };

  # TODO if doing a fresh install, set UID and GID
  users.users.ryan = {
    isNormalUser = true;
    extraGroups = [ "wheel" "audio" "lp" "plugdev" "docker" ];
    shell = pkgs.zsh;
  };

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
  };

  fileSystems."/home/ryan/calibre" = {
    device = "//192.168.86.31/calibre";
    fsType = "cifs";
    options = let
      # this line prevents hanging on network split
      automount_opts =
        "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";

    in [
      "${automount_opts},credentials=/etc/nixos/smb-secrets,uid=1000,gid=100"
    ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?
}

