{
  config,
  lib,
  pkgs,
  rev,
  dirtyRev,
  ...
}: {
  # imports = [../../modules];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [neovim];

  # we use Determinate Nix on macOS, so we need to turn off nix-darwin's daemon
  nix.enable = false;

  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes pipe-operators";

  security.pam.services.sudo_local.touchIdAuth = true;

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
