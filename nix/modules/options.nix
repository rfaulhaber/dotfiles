{ config, pkgs, options, lib, home-manager, ... }:

with lib;
with lib.my;

{
  options = with types; {
    user = mkOption {
      description = "Name of the primary account.";
      default = { };
      type = attrs;
    };
    userInfo = mkOption {
      description = "Additional user info associated with the user.";
      default = { };
      type = attrs;
    };
    home = {
      accounts = mkOptDesc attrs { }
        "Accounts managed by home-manager. Used primarily for email";
      configFile = mkOptDesc attrs { } "Files to place in $XDG_CONFIG_HOME";
      dataFile = mkOptDesc attrs { } "Files to place in $XDG_DATA_HOME";
      file = mkOptDesc attrs { } "Files to place directly in $HOME";
      packages = mkOptDesc attrs [ ] "User-level installed packages";
      programs =
        mkOptDesc attrs { } "Programs managed directly from home-manager";
      services =
        mkOptDesc attrs { } "Services managed directly from home-manager";
      xsession =
        mkOptDesc attrs { } "Xsession settings managed from home-manager";
    };

    env = mkOption {
      type = attrsOf (oneOf [ str path (listOf (either str path)) ]);
      apply = mapAttrs (n: v:
        if isList v then
          concatMapStringsSep ":" (x: toString x) v
        else
          (toString v));
      default = { };
      description = "";
    };

    dotfiles = {
      dir = mkOpt path (findFirst pathExists (toString ../../.) [
        "${config.user.home}/.config/dotfiles"
        "/etc/dotfiles"
      ]);
      binDir = mkOpt path "${config.dotfiles.dir}/bin";
      configDir = mkOpt path "${config.dotfiles.dir}/config";
      modulesDir = mkOpt path "${config.dotfiles.dir}/modules";
      themesDir = mkOpt path "${config.dotfiles.dir}/themes";
      emacsDir = mkOpt path "${config.dotfiles.dir}/doom.d";
    };
  };

  config = {
    user = rec {
      name = "ryan";
      description = "ryan";
      # TODO do better
      extraGroups = [ "wheel" "audio" "lp" "plugdev" ];
      isNormalUser = true;
      home = "/home/${name}";
      group = "users";
      uid = 1000;
      # TODO if doing a fresh install, set UID and GID
      # gid = 1000;
    };

    # supplementary user info used throughout config
    userInfo = {
      fullName = "Ryan Faulhaber";
      primaryEmail = "ryf@sent.as";
      primaryGPGKey = "A2205925F3B6C5B96F26C3CB544650C5A306061B";
      location = {
        city = "Cleveland";
        state = "Ohio";
        country = "United States";
      };
    };

    users.groups = { plugdev = { }; };

    home-manager = {
      useUserPackages = true;

      users.${config.user.name} = {
        home = {
          file = mkAliasDefinitions options.home.file;
          stateVersion = config.system.stateVersion;
        };

        accounts = mkAliasDefinitions options.home.accounts;
        home.packages = mkAliasDefinitions options.home.packages;
        programs = mkAliasDefinitions options.home.programs;
        services = mkAliasDefinitions options.home.services;
        xsession = mkAliasDefinitions options.home.xsession;

        xdg = {
          configFile = mkAliasDefinitions options.home.configFile;
          dataFile = mkAliasDefinitions options.home.dataFile;
        };
      };
    };

    users.users.${config.user.name} = mkAliasDefinitions options.user;

    # All my machines are in the same timezone
    # TODO should this be here?
    time = {
      timeZone = "America/New_York";
      hardwareClockInLocalTime = true;
    };

    # TODO where should these live?
    i18n.defaultLocale = "en_US.UTF-8";
    console = {
      font = "Lat2-Terminus16";
      keyMap = "us";
    };

    # TODO make standard nix module?
    nix = {
      package = pkgs.nixFlakes;
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
      gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 7d";
      };

      settings = let users = [ "root" config.user.name ];
      in {
        trusted-users = users;
        allowed-users = users;
        auto-optimise-store = true;
      };
    };

    nixpkgs.config.allowUnfree = true;

    system.stateVersion = "22.11";
  };
}
