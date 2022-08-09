{ config, lib, pkgs, home-manager, ... }:

{
  config = {
    console = {
      font = "Lat2-Terminus16";
      keyMap = "us";
    };

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

    home-manager.users.${config.user.name}.xdg = { enable = true; };

    environment = {
      sessionVariables = {
        XDG_CACHE_HOME = "$HOME/.cache";
        XDG_CONFIG_HOME = "$HOME/.config";
        XDG_DATA_HOME = "$HOME/.local/share";
        XDG_BIN_HOME = "$HOME/.local/bin";
      };
    };
  };
}
