{
  config,
  pkgs,
  options,
  lib,
  home-manager,
  isLinux,
  isDarwin,
  ...
}: let
  inherit (lib) types mkOption mkAliasDefinitions optionalAttrs concatMapStringsSep isList mapAttrs findFirst pathExists;
  inherit (lib.my) mkOpt mkOptDesc;
  inherit (types) attrs attrsOf oneOf str path listOf either;
in {
  options = {
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
      xsession =
        mkOptDesc attrs {} "Xsession settings managed from home-manager";
      wayland =
        mkOptDesc attrs {} "Wayland settings managed from home-manager";
      xdg =
        mkOptDesc attrs {} "XDG config files to write to directly.";
      dconf.settings =
        mkOptDesc attrs {} "dconf config from home-manager";
      autostart = mkOptDesc attrs {} "Things to automatically start for xdg autostart";
    };

    env = mkOption {
      type = attrsOf (oneOf [str path (listOf (either str path))]);
      apply = mapAttrs (n: v:
        if isList v
        then concatMapStringsSep ":" (x: toString x) v
        else (toString v));
      default = {};
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
    user = let
      name = "ryan";
    in
      {
        inherit name;
      }
      // lib.optionalAttrs isLinux {
        description = "ryan";
        # TODO do better
        extraGroups = ["wheel" "audio" "lp" "plugdev"];
        isNormalUser = true;
        home = "/home/${name}";
        group = "users";
        uid = 1000;
        # TODO if doing a fresh install, set UID and GID
        # gid = 1000;
      }
      // lib.optionalAttrs isDarwin {
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

    users.groups = lib.mkIf pkgs.stdenv.isLinux {plugdev = {};};

    home-manager = {
      useUserPackages = true;

      users.${config.user.name} =
        {
          home = {
            file = mkAliasDefinitions options.home.file;
            packages = mkAliasDefinitions options.home.packages;
            stateVersion =
              if isLinux
              then config.system.stateVersion
              else "25.05";
          };

          accounts = mkAliasDefinitions options.home.accounts;
          programs = mkAliasDefinitions options.home.programs;
          services = mkAliasDefinitions options.home.services;
        }
        // lib.optionalAttrs isLinux {
          xsession = mkAliasDefinitions options.home.xsession;
          dconf.settings = mkAliasDefinitions options.home.dconf.settings;

          xdg = {
            configFile = mkAliasDefinitions options.home.configFile;
            dataFile = mkAliasDefinitions options.home.dataFile;
            # TODO can I just forward all this stuff automatically? it's really annoying!
            autostart = mkAliasDefinitions options.home.autostart;
          };
        };
    };

    users.users.${config.user.name} = mkAliasDefinitions options.user;
  };
}
