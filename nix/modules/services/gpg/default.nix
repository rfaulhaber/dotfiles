{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.gpg;
in {
  options.modules.services.gpg = {
    enable = mkEnableOption false;
  };

  config = mkIf cfg.enable {
    # it is unclear to me how to automatically unlock gnome keyring upon login, so
    # I'm taking the shotgun approach
    services = {
      dbus.packages = with pkgs; [gnome.gnome-keyring gcr dconf];
      gnome = {gnome-keyring.enable = true;};
    };

    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "gnome3";
    };

    # adding this in to resolve an issue where "pass" doesn't work
    environment.systemPackages = with pkgs; [pinentry];
  };
}
