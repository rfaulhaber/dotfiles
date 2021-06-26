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
      direnv
      djvulibre
      epdfview
      graphviz
      imagemagick
      isync
      libtool # vterm
      libvterm
      mu
      nixfmt
      nodePackages.mermaid-cli
      sqlite
      wordnet
      xdotool
      xorg.xwininfo
      texlive.combined.scheme-medium
    ];

    environment.etc."xdg/mimeapps.list" = {
      text = ''
        [Default Applications]
        application/pdf=org.gnome.Evince.desktop;emacs.desktop
      '';
    };
  };
}
