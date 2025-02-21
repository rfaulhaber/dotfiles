# this module is not very, well, /modular/. it's pretty specific to my extremely
# specific emacs configuration I would like to change that!
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.programs.emacs;
  shellAliases = {
    ec = "emacsclient";
    eo = "emacsclient -n"; # "emacs open"
  };
in {
  options.modules.programs.emacs = {
    enable = mkEnableOption false;
    package = mkOption {
      description = "Emacs package to use.";
      type = types.package;
      default = pkgs.emacs;
    };
    mutableConfg = mkOption {
      description = "If enabled, links ~/Project/dotfiles/doom.d to .config rather than from /nix/store.";
      type = types.bool;
      default = false;
    };
    doomUnstraightened = mkOption {
      description = "If enabled, uses Nix Doom Unstraightened.";
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [
      (import inputs.emacs-overlay)
      inputs.nix-doom-emacs-unstraightened.overlays.default
    ];

    services.emacs = {
      enable = true;
      install = true;
      defaultEditor = true;
      package = with pkgs; ((emacsPackagesFor cfg.package).withPackages
        (epkgs:
          with epkgs; [
            pdf-tools
            prettier
            vterm
            tree-sitter
            tree-sitter-langs
            treesit-grammars.with-all-grammars # probably overkill but works for now
          ]));
    };

    # emacs dependency
    modules.programs.aspell.enable = true;

    # dependencies for my very specific configuration of doom
    # see doom.d/init.el for more
    # we need to include every program either directly or indirectly referenced in config
    # TODO can I rewrite this such that they're not all globally available?
    user.packages = with pkgs; [
      (mkIf (config.modules.services.mail.enable) mu)
      alejandra
      ast-grep
      clang # unfortunately we need a C compiler for various dependencies
      cmake
      direnv
      djvulibre
      editorconfig-core-c
      fd
      fzf
      git
      gnumake
      gnutls
      graphviz
      imagemagick
      languagetool
      nil
      nodePackages.mermaid-cli
      ripgrep
      sqlite
      texlive.combined.scheme-medium
      wordnet
      xdotool
      xorg.xprop
      xorg.xwininfo
      zstd
    ];

    environment.etc."xdg/mimeapps.list" = {
      text = ''
        [Default Applications]
        application/pdf=org.gnome.Evince.desktop;emacs.desktop
      '';
    };

    programs.zsh.shellAliases = mkIf config.modules.programs.zsh.enable shellAliases;

    home.programs.nushell.shellAliases = mkIf config.modules.programs.nushell.enable shellAliases;

    home.file.doomconfig = {
      source = config.dotfiles.emacsDir;
      target = "${config.user.home}/.config/doom";
    };
  };
}
