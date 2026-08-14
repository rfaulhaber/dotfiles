{
  inputs,
  config,
  pkgs,
  ...
}: {
  imports = [inputs.determinate.darwinModules.default];

  # NOTE darwin options
  # https://nix-darwin.github.io/nix-darwin/manual/index.html
  modules = {
    themes.active = "tokyo-night-dark";
    darwin = {
      airvpn.enable = true;
      dock = {
        enable = true;
      };
      random-wallpaper = {
        enable = true;
        token = config.sops.secrets.unsplash.path;
      };
    };
    programs = {
      btop.enable = true;
      claude.enable = true;
      emacs = {
        enable = true;
        # eos temporarily will use stable emacs for faster builds
        # package = pkgs.emacs-git;
        package = pkgs.emacs;
      };
      ghostty.enable = true;
      direnv.enable = true;
      zellij.enable = true;
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
      };
      sops = {
        enable = true;
        secrets = {
          unsplash = {
            owner = config.user.name;
            group = "staff"; # TODO more dynamic?
            mode = "0440";
          };
          # WireGuard config from AirVPN's Config Generator; root-only since
          # only `sudo wg-quick` reads it.
          "airvpn.conf" = {};
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
    inputs.rz.packages.${pkgs.stdenv.hostPlatform.system}.default
    feishin
  ];

  # Determinate Nix (its installer, not nix-darwin) owns /etc/nix/nix.conf on
  # macOS, so nix-darwin's daemon management stays off and plain nix.settings
  # is silently ignored. Custom settings must go through
  # determinateNix.customSettings, which renders /etc/nix/nix.custom.conf
  # (!include'd from the managed nix.conf).
  nix.enable = false;
  determinateNix.customSettings = {
    # Parallel evaluation; 0 = all cores (Determinate Nix caps this at 32).
    eval-cores = 0;
    # extra-: append to the managed nix.conf's defaults (nix-command, flakes)
    # rather than replacing them.
    extra-experimental-features = ["pipe-operators" "ca-derivations" "parallel-eval"];
  };

  # nixpkgs.config.contentAddressedByDefault = true;

  # TODO move
  security.pam.services.sudo_local.touchIdAuth = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";
}
