{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules;
in {
  config = mkIf pkgs.stdenv.isDarwin {
    # Set primary user for user-specific options
    system.primaryUser = "ryan";

    # Use a stable state version
    system.stateVersion = 4;

    # Darwin-specific Nix configuration
    nix = {
      package = pkgs.nix;
      gc = {
        automatic = true;
        options = "--delete-older-than 7d";
      };
      settings = {
        trusted-users = ["root" "@admin"];
        allowed-users = ["root" "@admin"];
        experimental-features = ["nix-command" "flakes" "pipe-operators"];
      };

      # Enable automatic store optimization (replaces auto-optimise-store)
      optimise.automatic = true;
    };
  };
}
# Platform is set by the host configuration
# Basic system packages that work well on Darwin
# environment.systemPackages = with pkgs; [
#   # Core utilities that improve the macOS experience
#   coreutils
#   findutils
#   gnugrep
#   gnused
#   gawk
#   # Development tools
#   git
#   curl
#   wget
#   # Text processing
#   ripgrep
#   fd
#   # Archive tools
#   unzip
#   gzip
#   # Modern replacements for common tools
#   bat
#   eza
#   dust
#   duf
# ];
# Configure system fonts
# fonts.packages = with pkgs; [
#   # Programming fonts
#   fira-code
#   fira-code-symbols
#   jetbrains-mono
#   source-code-pro
#   # System fonts
#   inter
#   roboto
#   open-sans
# ];
# System preferences via nix-darwin
# system.defaults = {
# Dock configuration
# dock = {
#   autohide = true;
#   orientation = "bottom";
#   tilesize = 48;
#   show-recents = false;
#   # Remove default apps from dock
#   persistent-apps = [];
# };
# # Finder configuration
# finder = {
#   AppleShowAllExtensions = true;
#   ShowPathbar = true;
#   ShowStatusBar = true;
#   FXDefaultSearchScope = "SCcf"; # Search current folder
#   FXEnableExtensionChangeWarning = false;
# };
# NSGlobalDomain (system-wide) preferences
# NSGlobalDomain = {
#   # Key repeat settings
#   InitialKeyRepeat = 15;
#   KeyRepeat = 2;
#   # Disable automatic text substitutions
#   NSAutomaticCapitalizationEnabled = false;
#   NSAutomaticDashSubstitutionEnabled = false;
#   NSAutomaticPeriodSubstitutionEnabled = false;
#   NSAutomaticQuoteSubstitutionEnabled = false;
#   NSAutomaticSpellingCorrectionEnabled = false;
#   # Show file extensions
#   AppleShowAllExtensions = true;
#   # Disable press-and-hold for accent characters
#   ApplePressAndHoldEnabled = false;
# };
# Trackpad configuration
# trackpad = {
#   Clicking = true; # Tap to click
#   TrackpadThreeFingerDrag = true;
# };
# Menu bar configuration (temporarily disabled - option may not exist in current nix-darwin)
# menuExtrasClock = {
#   ShowSeconds = true;
#   Show24Hour = true;
# };
# };
# Keyboard configuration
# system.keyboard = {
#   enableKeyMapping = true;
#   remapCapsLockToControl = true;
# };
# Security settings (updated option name)
#   security.pam.services.sudo_local.touchIdAuth = true;
# };
# }

