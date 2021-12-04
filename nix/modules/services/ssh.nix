{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.services.ssh;
in {
  options.modules.services.ssh = {
    enable = mkEnableOption false;
    keys = mkOption {
      description = "SSH keys used by the default user of this machine.";
      default = [ ];
      type = types.listOf types.str;
    };

  };

  config = mkIf cfg.enable {
    services.openssh = {
      enable = true;
      passwordAuthentication = false;
    };

    user.openssh.authorizedKeys.keys = cfg.keys;
  };
}
