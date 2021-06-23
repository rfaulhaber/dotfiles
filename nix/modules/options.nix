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
    home = {
      configFile = mkOptDesc attrs { } "Files to place in $XDG_CONFIG_HOME";
      dataFile = mkOptDesc attrs { } "Files to place in $XDG_DATA_HOME";
      file = mkOptDesc attrs { } "Files to place directly in $HOME";
      packages = mkOptDesc attrs [ ] "User-level installed packages";
      programs =
        mkOptDesc attrs { } "Programs managed directly from home-manager";
      services =
        mkOptDesc attrs { } "Services managed directly from home-manager";
      bspwm = mkOptDesc attrs { } "bspwm config options";
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

    # thank you again hlissner
    dotfiles = let t = either str path;
    in {
      dir = mkOpt t (findFirst pathExists (toString ../.)
        [ "${config.user.home}/.config/dotfiles" ]);
      binDir = mkOpt t "${config.dotfiles.dir}/bin";
      configDir = mkOpt t "${config.dotfiles.dir}/config";
      modulesDir = mkOpt t "${config.dotfiles.dir}/modules";
      themesDir = mkOpt t "${config.dotfiles.modulesDir}/themes";
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

    users.groups = { plugdev = { }; };

    home-manager = {
      useUserPackages = true;

      users.${config.user.name} = {
        home = {
          file = mkAliasDefinitions options.home.file;
          stateVersion = "21.05";
        };

        home.packages = mkAliasDefinitions options.home.packages;
        programs = mkAliasDefinitions options.home.programs;
        services = mkAliasDefinitions options.home.services;
        xsession.windowManager.bspwm = mkAliasDefinitions options.home.bspwm;

        xdg = {
          configFile = mkAliasDefinitions options.home.configFile;
          dataFile = mkAliasDefinitions options.home.dataFile;
        };
      };
    };

    users.users.${config.user.name} = mkAliasDefinitions options.user;

    nix = let users = [ "root" config.user.name ];
    in {
      trustedUsers = users;
      allowedUsers = users;
    };
  };
}
