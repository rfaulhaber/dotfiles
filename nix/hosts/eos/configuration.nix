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
    nix = {
      bigHost = true;
      substituters.enable = true;
    };
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
      claude = {
        enable = true;
        memoryDir = "~/.claude-memory";
      };
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
      mcp.servers = {
        codegraph.agents.claude = true;
        ebay.agents.claude = true;
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
          # eBay developer keyset the local eBay MCP server mints its
          # application tokens from
          "ebay-mcp/client-id" = {
            owner = config.user.name;
            group = "staff";
            mode = "0400";
          };
          "ebay-mcp/client-secret" = {
            owner = config.user.name;
            group = "staff";
            mode = "0400";
          };
        };
      };
    };
    services = {
      gpg.enable = true;
      ssh.client = {
        enable = true;
        deployAgent.enable = true;
      };
    };
  };

  # TODO add place for ad-hoc packages?
  user.packages = with pkgs; [
    inputs.rz.packages.${pkgs.stdenv.hostPlatform.system}.default
    feishin
    gh
  ];

  # Determinate Nix (its installer, not nix-darwin) owns /etc/nix/nix.conf on
  # macOS, so nix-darwin daemon management stays off and plain nix.settings is
  # evaluated and then silently discarded. modules/conf.nix detects isDarwin
  # and routes the shared baseline (trusted-users, substituters,
  # experimental-features) through determinateNix.customSettings instead,
  # which renders the nix.custom.conf that the managed nix.conf includes.

  # nixpkgs.config.contentAddressedByDefault = true;

  # TODO move
  security.pam.services.sudo_local.touchIdAuth = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";
}
