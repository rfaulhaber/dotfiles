{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.services.passwords;
in {
  options.modules.services.passwords = { enable = mkEnableOption false; };

  config = mkIf cfg.enable {
    # TODO fill me out!

    assertions = [{
      assertion = config.services.xserver.enable;
      message = "This module must be used with xserver";
    }];

    programs.seahorse.enable = true;

    security.pam.services.lightdm.enableGnomeKeyring = true;

    home.xsession.profileExtra = ''
      eval $(${pkgs.gnome.gnome-keyring}/bin/gnome-keyring --start --daemonize)
      export SSH_AUTH_SOCK
    '';
  };
}
