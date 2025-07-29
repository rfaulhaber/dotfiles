{
  config,
  lib,
  pkgs,
  isLinux,
  isDarwin,
  ...
}:
with lib; let
  isWayland = isLinux && config.modules.desktop.environment.isWayland;
  cfg = config.modules.services.gpg;
in {
  options.modules.services.gpg = {
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

      # user.packages = with pkgs;
      #   lib.optionals isDarwin [
      #     gnupg
      #   ];
    }
    // lib.optionalAttrs isLinux {
      security.pam.services.login.enableGnomeKeyring = true;
    };
}
