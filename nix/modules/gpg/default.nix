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
          pinentryPackage = pkgs.pinentry-gnome3;
        };

      # somehow, for some reason, programs.gnupg.agent.enable does not imply a gpg installation on darwin
      user.packages = with pkgs;
        lib.optionals isDarwin [
          gnupg
          pinentry_mac
        ];

      home.file.gpgconf = mkIf isDarwin {
        text = "pinentry-program /etc/profiles/per-user/${config.user.name}/bin/pinentry-mac";
        target = "${config.user.home}/.gnupg/gpg-agent.conf";
      };
    }
    // lib.optionalAttrs isLinux {
      security.pam.services.login.enableGnomeKeyring = true;
    };
}
