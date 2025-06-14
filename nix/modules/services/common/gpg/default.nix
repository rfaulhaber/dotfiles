{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  inherit (config.modules.desktop.environment) isWayland;
  cfg = config.modules.services.gpg;
in {
  options.modules.services.gpg = {
    enable = mkEnableOption false;
  };

  config = mkIf cfg.enable (mkMerge [
    # GPG agent configuration (works on both Linux and Darwin)
    {
      programs.gnupg.agent =
        {
          enable = true;
          enableSSHSupport = true;
        }
        // lib.optionalAttrs pkgs.stdenv.isLinux {
          # Linux-specific GPG options
          enableBrowserSocket = true;
          pinentryPackage = mkIf isWayland pkgs.pinentry-gnome3;
        }
        // lib.optionalAttrs pkgs.stdenv.isDarwin {
          # Darwin-specific GPG options (if any)
          # pinentryPackage might not be available on Darwin
        };
    }

    # Linux-specific GNOME keyring setup
    (mkIf pkgs.stdenv.isLinux {
      # it is unclear to me how to automatically unlock gnome keyring upon login, so
      # I'm taking the shotgun approach
      # Linux-specific services temporarily disabled for Darwin compatibility debugging
      # services = {
      #   dbus.packages = with pkgs; [
      #     gnome-keyring
      #     gcr
      #     dconf
      #   ];
      #   gnome.gnome-keyring.enable = true;
      # };

      # Temporarily disabled for Darwin compatibility debugging
      # security.pam.services.login.enableGnomeKeyring = true;
    })
  ]);
}
