{
  pkgs,
  lib,
  config,
  options,
  ...
}:
with lib;
with lib.my; {
  options = with types; {
    user = mkOption {
      description = "Name of the primary account.";
      default = {};
      type = attrs;
    };
    userInfo = mkOption {
      description = "Additional user info associated with the user.";
      default = {};
      type = attrs;
    };
    home = {
      accounts =
        mkOptDesc attrs {}
        "Accounts managed by home-manager. Used primarily for email";
      configFile = mkOptDesc attrs {} "Files to place in $XDG_CONFIG_HOME";
      dataFile = mkOptDesc attrs {} "Files to place in $XDG_DATA_HOME";
      file = mkOptDesc attrs {} "Files to place directly in $HOME";
      packages = mkOptDesc attrs [] "User-level installed packages";
      programs =
        mkOptDesc attrs {} "Programs managed directly from home-manager";
      services =
        mkOptDesc attrs {} "Services managed directly from home-manager";
    };

    dotfiles = {
      dir = mkOpt path (findFirst pathExists (toString ../../.) [
        "${config.user.home}/.config/dotfiles"
      ]);
      binDir = mkOpt path "${config.dotfiles.dir}/bin";
      configDir = mkOpt path "${config.dotfiles.dir}/config";
      modulesDir = mkOpt path "${config.dotfiles.dir}/modules";
      themesDir = mkOpt path "${config.dotfiles.dir}/themes";
      emacsDir = mkOpt path "${config.dotfiles.dir}/doom.d";
    };
  };

  config = {
    users.users.${config.user.name} = mkAliasDefinitions options.user;

    user = rec {
      name = "ryan";
      home = "/Users/${name}";
    };

    # supplementary user info used throughout config
    userInfo = {
      fullName = "Ryan Faulhaber";
      primaryEmail = "ryf@sent.as";
      primaryGPGKey = "A90BC7B722983F6BB8EAC1DA144A6B5FBB68FC9D";
      location = {
        city = "Cleveland";
        state = "Ohio";
        country = "United States";
        latitude = 41.49;
        longitude = -81.69;
      };
    };

    home-manager = {
      useUserPackages = true;

      users.${config.user.name} = {
        home = {
          file = mkAliasDefinitions options.home.file;
          stateVersion = "25.11";
        };

        accounts = mkAliasDefinitions options.home.accounts;
        home.packages = mkAliasDefinitions options.home.packages;
        programs = mkAliasDefinitions options.home.programs;
        services = mkAliasDefinitions options.home.services;
      };
    };
  };
}
