{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.programs.emacs;
in {
  options.modules.programs.emacs = {
    enable = mkEnableOption false;
    useNativeComp = mkOption {
      description = "Uses emacs28NativeComp package instead of default Emacs.";
      type = types.bool;
      default = false;
    };
  };
  config = mkIf cfg.enable {
    services.emacs = {
      enable = true;
      install = true;
      defaultEditor = true;
      package = with pkgs;
        if cfg.useNativeComp then emacs28NativeComp else emacs;
    };

    # dependencies for my very specific configuration of doom
    # see doom.d/init.el for more
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

    programs.zsh.shellAliases = {
      ec = "emacsclient";
      eo = "emacsclient -n"; # "emacs open"
    };
  };
}
