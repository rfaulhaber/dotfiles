{
  config,
  pkgs,
  options,
  lib,
  isLinux,
  isDarwin,
  ...
}: let
  inherit (lib) types mkOption mkAliasDefinitions optionalAttrs concatMapStringsSep isList mapAttrs findFirst pathExists;
  inherit (lib.my) mkOpt mkOptDesc;
  inherit (types) attrs attrsOf oneOf str path listOf either;
in {
  options = {
    # `attrs` merges shallowly: if two modules set the same top-level key
    # under `user` (or under `home.*` below), the later definition wins
    # outright rather than merging — a silent clobber, not a conflict error.
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
    # Same shallow-merge caveat as `user` above — each of these is forwarded
    # into a single home-manager option via mkAliasDefinitions, so modules
    # sharing a top-level key here clobber each other silently too.
    home = {
      configFile = mkOptDesc attrs {} "Files to place in $XDG_CONFIG_HOME (aliased to home-manager xdg.configFile).";
      file = mkOptDesc attrs {} "Files to place directly in $HOME (aliased to home-manager home.file).";
      programs = mkOptDesc attrs {} "Programs managed directly from home-manager.";
      services = mkOptDesc attrs {} "Services managed directly from home-manager.";
      dconf.settings = mkOptDesc attrs {} "dconf config from home-manager (Linux only).";
      autostart = mkOptDesc attrs {} "XDG autostart entries (Linux only).";
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
            stateVersion =
              if isLinux
              then config.system.stateVersion
              else "25.05";
          };

          programs = mkAliasDefinitions options.home.programs;
          services = mkAliasDefinitions options.home.services;

          # home-manager's manual builds an options.json from module
          # declarations, embedding the flake `-source` store path into the
          # closure — the same non-reproducibility leak as the NixOS manual.
          # We don't ship the per-user manpage, so drop it.
          manual.manpages.enable = false;
        }
        // {
          xdg =
            {
              configFile = mkAliasDefinitions options.home.configFile;
            }
            // lib.optionalAttrs isLinux {
              autostart = mkAliasDefinitions options.home.autostart;
            };
        }
        // lib.optionalAttrs isLinux {
          dconf.settings = mkAliasDefinitions options.home.dconf.settings;
        };
    };

    users.users.${config.user.name} = mkAliasDefinitions options.user;
  };
}
