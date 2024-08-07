{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.passwords;
  lightdm = config.modules.desktop.lightdm;
in {
  options.modules.services.passwords = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.services.xserver.enable;
        message = "This module must be used with xserver";
      }
    ];

    programs.seahorse.enable = true;

    security.pam.services.lightdm.enableGnomeKeyring = mkIf lightdm.enable true;

    home.services.gnome-keyring = {
      enable = true;
      components = ["ssh" "secrets" "pkcs11"];
    };
  };
}
