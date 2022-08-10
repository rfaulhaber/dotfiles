{ config, lib, pkgs, ... }:

with lib;

let cfg = config.common.programs.emacs;
in {
  options.common.programs.emacs.enable = mkEnableOption false;
  config = mkIf cfg.enable {
    home.services.emacs = {
      enable = true;
      install = true;
      defaultEditor = true;
      package = pkgs.emacs28NativeComp;
    };

    # dependencies for my very specific configuration of doom
    # see doom.d/init.el for more
    environment.systemPackages = with pkgs; [
      # (mkIf (config.common.services.mail.enable) mu)
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
      git
      gnumake
      gnutls
      graphviz
      imagemagick
      languagetool
      nixfmt
      nodePackages.mermaid-cli
      ripgrep
      sqlite
      texlive.combined.scheme-medium
      wordnet
      xdotool
      xorg.xwininfo
      zstd
    ];

    environment.etc."xdg/mimeapps.list" = {
      text = ''
        [Default Applications]
        application/pdf=org.gnome.Evince.desktop;emacs.desktop
      '';
    };

    home.programs.zsh.shellAliases = {
      ec = "emacsclient";
      eo = "emacsclient -n"; # "emacs open"
    };
  };
}
