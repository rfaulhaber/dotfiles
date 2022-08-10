{ config, pkgs, options, lib, home-manager, ... }:

with lib;
with lib.my;

{
  imports = [ ../common/options.nix ];
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
