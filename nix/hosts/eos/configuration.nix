{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [../../modules];

  # # Define the user info that modules will use
  # userInfo = {
  #   fullName = "Ryan Faulhaber";
  #   primaryEmail = "ryan@faulhaber.io";
  #   primaryGPGKey = "6B51D9B8CEA6A53C";
  # };

  # Define the main user configuration
  # user = {
  #   name = "ryan";
  #   description = "Ryan Faulhaber";
  # };

  modules = {
    programs = {
      emacs.enable = true;
      git = {
        enable = true;
        useDelta = true;
      };
      nushell = {
        enable = true;
        setDefault = true;
        zoxide.enable = true;
        carapace.enable = true;
      };
      wezterm.enable = true;
      direnv.enable = true;
    };

    # Services that work on Darwin
    # services = {
    #   gpg.enable = true;
    # };

    # Theme configuration
    themes.active = "tokyo-night-dark";
  };
}
