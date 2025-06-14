{
  config,
  pkgs,
  options,
  lib,
  home-manager,
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
      xsession =
        mkOptDesc attrs {} "Xsession settings managed from home-manager";
      wayland =
        mkOptDesc attrs {} "Wayland settings managed from home-manager";
      xdg =
        mkOptDesc attrs {} "XDG config files to write to directly.";
      dconf.settings =
        mkOptDesc attrs {} "dconf config from home-manager";
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
      dir = mkOpt path (toString ../../.);
      binDir = mkOpt path "${config.dotfiles.dir}/bin";
      configDir = mkOpt path "${config.dotfiles.dir}/config";
      modulesDir = mkOpt path "${config.dotfiles.dir}/modules";
      themesDir = mkOpt path "${config.dotfiles.dir}/themes";
      emacsDir = mkOpt path "${config.dotfiles.dir}/doom.d";
    };
  };

  config = {
    user = let
      common = {
        name = "ryan";
        description = "ryan";
      };
    in
      mkMerge [
        common
        (mkIf pkgs.stdenv.isLinux {
          # TODO do better
          extraGroups = ["wheel" "audio" "lp" "plugdev"];
          isNormalUser = true;
          home = "/home/${common.name}";
          group = "users";
          uid = 1000;
          # TODO if doing a fresh install, set UID and GID
          # gid = 1000;
        })
        (mkIf pkgs.stdenv.isDarwin {
          home = "/Users/${common.name}";
        })
      ];

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

    users.groups = mkIf pkgs.stdenv.isLinux {plugdev = {};};

    home-manager = {
      useUserPackages = true;

      users.${config.user.name} = {
        home = {
          file = mkAliasDefinitions options.home.file;
          username = mkForce config.user.name;
          homeDirectory = mkForce (
            if pkgs.stdenv.isDarwin
            then "/Users/ryan"
            else config.user.home
          );
          stateVersion =
            if pkgs.stdenv.isDarwin
            then "25.11"
            else config.system.stateVersion;
        };

        accounts = mkAliasDefinitions options.home.accounts;
        home.packages = mkAliasDefinitions options.home.packages;
        programs = mkAliasDefinitions options.home.programs;
        services = mkAliasDefinitions options.home.services;
        xsession = mkAliasDefinitions options.home.xsession;
        dconf.settings = mkAliasDefinitions options.home.dconf.settings;

        xdg = {
          configFile = mkAliasDefinitions options.home.configFile;
          dataFile = mkAliasDefinitions options.home.dataFile;
        };
      };
    };

    users.users.${config.user.name} = mkIf pkgs.stdenv.isLinux (mkAliasDefinitions options.user);

    # Platform compatibility assertions
    assertions = [
      {
        assertion = !(config.modules.desktop.enable or false && pkgs.stdenv.isDarwin);
        message = "Desktop environment modules are only supported on Linux";
      }
      {
        assertion = !(config.modules.services.zfs.enable or false && pkgs.stdenv.isDarwin);
        message = "ZFS services are only supported on Linux";
      }
      {
        assertion = !(config.modules.services.docker.enable or false && pkgs.stdenv.isDarwin);
        message = "Docker service module is Linux-only (use Docker Desktop manually on macOS)";
      }
    ];
  };
}
