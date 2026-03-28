{
  inputs,
  config,
  lib,
  pkgs,
  ...
}: {
  # NOTE darwin options
  # https://nix-darwin.github.io/nix-darwin/manual/index.html
  modules = {
    themes.active = "tokyo-night-dark";
    darwin = {
      dock = {
        enable = true;
      };
      random-wallpaper = {
        enable = true;
        token = config.sops.secrets.unsplash.path;
      };
    };
    programs = {
      claude.enable = true;
      emacs = {
        enable = true;
        # eos temporarily will use stable emacs for faster builds
        # package = pkgs.emacs-git;
        package = pkgs.emacs;
        doomUnstraightened = {
          enable = true;
          setDefault = true;
        };
      };
      neovim.enable = true;
      ghostty.enable = true;
      direnv.enable = true;
      git = {
        enable = true;
        useDelta = true;
        useJJ = true;
      };
      nushell = {
        enable = true;
        setDefault = true;
        zoxide.enable = true;
        carapace.enable = true;
        plugins = with pkgs.nushellPlugins; [
          polars
        ];
      };
      sops = {
        enable = true;
        secrets = {
          unsplash = {
            owner = config.user.name;
            group = "staff"; # TODO more dynamic?
            mode = "0440";
          };
        };
      };
    };
    services = {
      gpg.enable = true;
      ssh.client.enable = true;
    };
  };

  # TODO add place for ad-hoc packages?
  user.packages = with pkgs; [
    feishin
  ];

  home-manager.backupFileExtension = "home-manager";

  # we use Determinate Nix on macOS, so we need to turn off nix-darwin's daemon
  nix.enable = false;

  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes pipe-operators ca-derivations";

  # nixpkgs.config.contentAddressedByDefault = true;

  # TODO move
  security.pam.services.sudo_local.touchIdAuth = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";
}
