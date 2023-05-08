{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.emacs;
in {
  options.modules.programs.emacs.enable = mkEnableOption false;
  config = mkIf cfg.enable {
    services.emacs = {
      enable = true;
      install = true;
      defaultEditor = true;
      package = with pkgs; ((emacsPackagesFor emacs).emacsWithPackages
        (epkgs: with epkgs; [pdf-tools vterm]));
    };

    # dependencies for my very specific configuration of doom
    # see doom.d/init.el for more
    # we need to include every program either directly or indirectly referenced in config
    environment.systemPackages = with pkgs; [
      (mkIf (config.modules.services.mail.enable) mu)
      alejandra # nix formatter
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

    programs.zsh.shellAliases = {
      ec = "emacsclient";
      eo = "emacsclient -n"; # "emacs open"
    };
  };
}
