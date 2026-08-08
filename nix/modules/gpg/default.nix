{
  config,
  lib,
  pkgs,
  isLinux,
  isDarwin,
  ...
}: let
  inherit (lib) mkEnableOption mkIf optionalAttrs optionals;
  cfg = config.modules.services.gpg;
in {
  options.modules.services.gpg = {
    enable = mkEnableOption false;
  };

  # Unlocking on Linux runs through pinentry, not through any gpg component of
  # the keyring — gnome-keyring has not had one for years. pinentry-gnome3 links
  # libsecret, so when gpg-agent hands it SETKEYINFO it looks the passphrase up
  # in the Secret Service under schema org.gnupg.Passphrase, keyed by the
  # cache-mode-prefixed keyinfo ("n/<keygrip>", "s/<keygrip>" for ssh). A hit is
  # returned with no dialog, which is why unlocking the keyring is what stops
  # the prompts. gnome-keyring serves that Secret Service, so it is required
  # here rather than merely a desktop nicety.
  #
  # Nothing else is needed: the dbus registrations for gnome-keyring and gcr
  # come from services.gnome.gnome-keyring and from the nixpkgs gnupg module
  # (which adds gcr for any gnome3-flavored pinentry), and that same module
  # already sets security.pam.services.login.enableGnomeKeyring.
  config = mkIf cfg.enable ({
      programs.gnupg.agent =
        {
          enable = true;
          enableSSHSupport = true;
        }
        // optionalAttrs isLinux {
          enableBrowserSocket = true;
          pinentryPackage = pkgs.pinentry-gnome3;
        };

      # somehow, for some reason, programs.gnupg.agent.enable does not imply a gpg installation on darwin
      user.packages = optionals isDarwin [
        pkgs.gnupg
        pkgs.pinentry_mac
      ];

      home.file.gpgconf = mkIf isDarwin {
        text = "pinentry-program /etc/profiles/per-user/${config.user.name}/bin/pinentry-mac";
        target = "${config.user.home}/.gnupg/gpg-agent.conf";
      };
    }
    # merged inside the mkIf call, not onto its result: `mkIf c {...} // {...}`
    # yields {_type="if"; condition; content;} plus stray keys that
    # dischargeProperties never reads, so the extra attrs vanish silently.
    // optionalAttrs isLinux {
      services.gnome.gnome-keyring.enable = true;
    });
}
