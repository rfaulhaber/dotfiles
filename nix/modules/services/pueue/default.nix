{ config, lib, pkgs, ... }:

let cfg = config.modules.services.pueue;
in {
  options.modules.services.pueue = {
    enable = lib.mkEnableOption false;
    supportedUsers = lib.mkOption {
      description = "Users allowed to access the Pueue daemon.";
      type = lib.types.listOf lib.types.str;
      default = [config.user.name "root"];
    };
  };

  # TODO to support multi-user:
  # - create custom config file, placing daemon socket in shared directory (/run?)
  # - create wrapper for pueue executable

  config = lib.mkIf cfg.enable {
    user.packages = with pkgs; [ pueue ];

    # taken from https://github.com/Nukesor/pueue/blob/main/utils/pueued.service
    # I don't like it being user-only, should find a way to make it available
    # for all users
    systemd.user.services.pueued = {
      description = "Starts and runs pueued.";
      wantedBy = ["default.target"];
      serviceConfig = {
        ExecStart = "${pkgs.pueue}/bin/pueued -vv";
        Restart = "no";
      };
    };
  };
}
