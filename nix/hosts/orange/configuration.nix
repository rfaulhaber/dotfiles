{ config, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    aspell
    bat
    coreutils
    direnv
    emacs
    exa
    fd
    findutils
    fzf
    gnused
    gnutar
    htop
    nixfmt
    pass
    ripgrep
    rust-analyzer
    tokei
    wget
    zsh
    zsh-autosuggestions
    zsh-syntax-highlighting
  ];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "~/Projects/dotfiles/nix/hosts/orange/configuration.nix";

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.

  # services.emacs = {
  #   enable = true;
  #   defaultEditor = true;
  # };

  programs.zsh = {
    enable = true;
    ohMyZsh = {
      enable = true;
      plugins = [ "git" "colored-man-pages" ];
      theme = "agnoster";
    };
    autosuggestions.enable = true;
    autosuggestions.highlightStyle = "fg=#41505E";
    syntaxHighlighting.enable = true;
    shellAliases = {
      pbcopy = "xclip -selection clipboard";
      pbpaste = "xclip -selection clipboard -o";
      vi = "nvim";
      vim = "nvim";
      ls = "exa";
      l = "exa -lah";
      ll = "exa -lh";
    };
  };
  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
