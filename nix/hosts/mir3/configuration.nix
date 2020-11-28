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
  ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.pulseaudio = true;

  boot.tmpOnTmpfs = true;
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
  environment.systemPackages = with pkgs; [
    oh-my-zsh
    zsh
    zsh-autosuggestions
    zsh-completions

    #desktop
    betterlockscreen
    bspwm
    calibre
    chromium
    discord
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
    pcloud
    polybarFull
    pulsemixer
    pavucontrol
    redshift
    rofi
    spotify
    sxhkd
    tdesktop
    xclip
    xscreensaver
    unstable.ripcord
    unstable.wally-cli

    #dev

    #dev.util
    coreutils-full
    docker
    gcc
    git
    lldb_11
    llvm_11
    rsync
    unstable.gdb
    unstable.gnumake

    #dev.js
    nodejs_latest
    unstable.nodePackages.typescript
    unstable.nodePackages.typescript-language-server
    nodePackages.yarn

    #dev.rust
    rustup

    #dev.racket
    unstable.racket

    #dev.editors
    neovim
    unstable.emacs

    nixfmt

    #util
    bat
    croc
    curl
    exa
    fd
    fzf
    gnupg
    htop
    jq
    pandoc
    unstable.ripgrep
    unstable.kvm
    unstable.qemu
    unstable.qemu-utils
    unstable.zoxide
    texlive.combined.scheme-medium
    unzip
    wget
    zip

    #emacs
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    cmake
    libtool # vterm
    libvterm
    sqlite
    unstable.mu
    unstable.isync

    #system deps
    python3
  ];

  environment.etc."xdg/mimeapps.list" = {
    text = ''
      [Default Applications]
      application/pdf=evince.desktop;emacs.desktop
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
    syntaxHighlighting.enable = true;
    shellAliases = {
      pbcopy = "xclip -selection clipboard";
      pbpaste = "xclip -selection clipboard -o";
      vi = "nvim";
      vim = "nvim";
      ls = "exa";
      l = "exa -lah";
      ll = "exa -lh";
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
    enableFontDir = true;
    enableGhostscriptFonts = true;
    enableDefaultFonts = true;
    fonts = with pkgs; [
      (unstable.nerdfonts.override { fonts = [ "Hack" ]; })
      font-awesome-ttf
      hack-font
      lato
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      noto-fonts-extra
      roboto
      ubuntu_font_family
    ];
    fontconfig.defaultFonts = {
      sansSerif = [ "Ubuntu Font Family" ];
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
          feh --bg-fill ~/Projects/dotfiles/nix/hosts/mir3/wallpapers/1.jpg
        '';
      };
      videoDrivers = [ "nvidia" ];
    };
    gnome3 = { gnome-keyring.enable = true; };
    dbus.packages = with pkgs; [ gnome3.gnome-keyring gcr gnome3.dconf ];

    # it is unclear to me how to automatically unlock gnome keyring upon login, so
    # I'm taking the shotgun approach

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
    config = { General = { Enable = "Source,Sink,Media,Socket"; }; };
  };

  users.groups = { plugdev = { }; };

  users.users.ryan = {
    isNormalUser = true;
    extraGroups = [ "wheel" "audio" "lp" "plugdev" ];
    shell = pkgs.zsh;
  };

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}

