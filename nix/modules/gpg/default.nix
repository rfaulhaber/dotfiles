{
  config,
  lib,
  pkgs,
  isLinux,
  ...
}:
with lib; let
  isWayland = isLinux && config.modules.desktop.environment;
  cfg = config.modules.gpg;
in {
  options.modules.gpg = {
    enable = mkEnableOption false;
  };

  config =
    mkIf cfg.enable {
      # it is unclear to me how to automatically unlock gnome keyring upon login, so
      # I'm taking the shotgun approach
      services = lib.optionalAttrs isLinux {
        dbus.packages = with pkgs; [
          gnome-keyring
          gcr
          dconf
        ];
        gnome.gnome-keyring.enable = true;
      };

      programs.gnupg.agent =
        {
          enable = true;
          enableSSHSupport = true;
        }
        // lib.optionalAttrs isLinux {
          enableBrowserSocket = true;
          pinentryPackage = lib.mkIf isWayland pkgs.pinentry-gnome3;
        };
    }
    // lib.optionalAttrs isLinux {
      security.pam.services.login.enableGnomeKeyring = true;
    };
}
