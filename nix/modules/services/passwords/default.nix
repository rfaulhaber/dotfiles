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

  config = mkIf cfg.enable (mkMerge [
    (mkIf config.modules.desktop.xserver.enable {
      programs.seahorse.enable = true;

      security.pam.services.lightdm.enableGnomeKeyring = mkIf lightdm.enable true;
    })
    {
      home.services.gnome-keyring = {
        enable = true;
        components = ["ssh" "secrets" "pkcs11"];
      };
    }
  ]);
}
