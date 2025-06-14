{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.passwords;
in {
  options.modules.services.passwords = {enable = mkEnableOption false;};

  config = mkIf cfg.enable (mkMerge [
    # Linux-specific desktop integration
    (mkIf pkgs.stdenv.isLinux {
      security.pam.services.lightdm.enableGnomeKeyring = mkIf (config.modules.desktop.lightdm.enable or false) true;
    })

    # Linux-specific programs (only available on Linux)
    (mkIf (pkgs.stdenv.isLinux && (config.modules.desktop.xserver.enable or false)) {
      programs.seahorse.enable = true;
    })

    # Cross-platform keyring service
    {
      home.services.gnome-keyring = {
        enable = true;
        components = ["ssh" "secrets" "pkcs11"];
      };
    }
  ]);
}
