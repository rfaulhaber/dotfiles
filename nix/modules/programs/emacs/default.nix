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

    environment.systemPackages = with pkgs; [
      #emacs
      aspell
      aspellDicts.en
      aspellDicts.en-computers
      aspellDicts.en-science
      cmake
      direnv
      djvulibre
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
    ];

    environment.etc."xdg/mimeapps.list" = {
      text = ''
        [Default Applications]
        application/pdf=org.gnome.Evince.desktop;emacs.desktop
      '';
    };
  };
}
