{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [../../modules];

  # Define the user info that modules will use
  userInfo = {
    fullName = "Ryan Faulhaber";
    primaryEmail = "ryan@faulhaber.io";
    primaryGPGKey = "6B51D9B8CEA6A53C";
  };

  # Define the main user configuration
  user = {
    name = "ryan";
    description = "Ryan Faulhaber";
  };

  # Module configuration
  modules = {
    # Programs that work well on Darwin
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
      starship.enable = true;
      direnv.enable = true;
      eza.enable = true;
      ghostty.enable = true;
    };

    # Services that work on Darwin
    services = {
      gpg.enable = true;
    };

    # Theme configuration
    themes.active = "tokyo-night-dark";
  };

  # Darwin-specific basic configuration is handled by modules/darwin.nix
  # Host-specific overrides can go here

  # Example: Custom environment variables for this host
  environment.variables = {
    EDITOR = "emacs";
    BROWSER = "open";
  };

  # Host-specific packages
  environment.systemPackages = with pkgs; [
    # Development tools
    neovim

    # System utilities
    htop
    tree

    # Network tools
    nmap

    # Fun stuff
    fortune
    cowsay
  ];
}
