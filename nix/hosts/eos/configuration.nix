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
    darwin = {
      dock = {
        enable = true;
      };
    };
    programs = {
      emacs = {
        enable = true;
        package = pkgs.emacs-git;
        doomUnstraightened = {
          enable = true;
          setDefault = true;
        };
      };
      neovim.enable = true;
      wezterm.enable = true;
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
    };
    services = {
      gpg.enable = true;
      ssh.client.enable = true;
    };
  };

  home-manager.backupFileExtension = "home-manager";

  # we use Determinate Nix on macOS, so we need to turn off nix-darwin's daemon
  nix.enable = false;

  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes pipe-operators";

  security.pam.services.sudo_local.touchIdAuth = true;

  # TODO move
  environment.shells = with pkgs; [nushell];

  # Enable alternative shell support in nix-darwin.
  # programs.fish.enable = true;

  # Set Git commit hash for darwin-version.
  # system.configurationRevision = rev || dirtyRev || null;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";
}
