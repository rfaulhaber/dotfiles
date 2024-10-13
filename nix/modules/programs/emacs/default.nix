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
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [
      (import inputs.emacs-overlay)
    ];
    services.emacs = {
      enable = true;
      install = true;
      defaultEditor = true;
      package = with pkgs; ((emacsPackagesFor cfg.package).withPackages
        (epkgs: with epkgs; [pdf-tools prettier vterm]));
    };

    # dependencies for my very specific configuration of doom
    # see doom.d/init.el for more
    # we need to include every program either directly or indirectly referenced in config
    # TODO can I rewrite this such that they're not all globally available?
    environment.systemPackages = with pkgs; [
      (mkIf (config.modules.services.mail.enable) mu)
      aspell
      aspellDicts.en
      aspellDicts.en-computers
      aspellDicts.en-science
      clang # unfortunately we need a C compiler for various dependencies
      cmake
      direnv
      djvulibre
      editorconfig-core-c
      epdfview
      fd
      fzf
      git
      gnumake
      gnutls
      graphviz
      imagemagick
      languagetool
      nodePackages.mermaid-cli
      pass
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

    programs.zsh.shellAliases = shellAliases;

    home.programs.nushell.shellAliases = shellAliases;
  };
}
