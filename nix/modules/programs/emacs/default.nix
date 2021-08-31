{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.programs.emacs;
in {
  options.modules.programs.emacs = { enable = mkEnableOption false; };
  config = mkIf cfg.enable {
    services.emacs = {
      enable = true;
      install = true;
      defaultEditor = true;
    };

    # dependencies for my very specific configuration of doom
    # see doom.d/init.el for more
    environment.systemPackages = with pkgs; [
      aspell
      aspellDicts.en
      aspellDicts.en-computers
      aspellDicts.en-science
      clang # unfortunately we need a C compiler for various dependencies
      cmake # dependency for vterm
      direnv
      djvulibre
      editorconfig-core-c
      epdfview
      fd
      git
      gnutls
      graphviz
      imagemagick
      isync
      languagetool
      libtool # needed by vterm
      libvterm # vterm
      mu
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
  };
}
