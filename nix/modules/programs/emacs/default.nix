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
  inherit (config.modules.desktop.environment) isX11;
  shellAliases = {
    ec = "emacsclient";
    eo = "emacsclient -n"; # "emacs open"
  };
  emacsPackages = epkgs:
    with epkgs; [
      pdf-tools
      prettier
      vterm
      tree-sitter
      tree-sitter-langs
      treesit-grammars.with-all-grammars # probably overkill but works for now
    ];

  # dependencies for my very specific configuration of doom
  # see doom.d/init.el for more
  # we need to include every program either directly or indirectly referenced in config
  # TODO can I rewrite this such that they're not all globally available?
  userPackages = with pkgs;
    [
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
      zstd
    ]
    ++ lib.optionals isX11 [
      xdotool
      xorg.xprop
      xorg.xwininfo
    ]
    ++ lib.optional config.modules.services.mail.enable mu;

  # TODO make unstraightened work lol
  unstraightenedPackage = emacsPkg:
    emacsPkg {
      emacs = cfg.package;
      # TODO point to something else?
      doomDir = ../../../../doom.d;
      doomLocalDir = "${config.user.home}/.local/share/nix-doom";
      extraPackages = emacsPackages;
      extraBinPackages = userPackages;
      experimentalFetchTree = true;
    };

  normalPackage = with pkgs; ((emacsPackagesFor cfg.package).withPackages emacsPackages);
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
    doomUnstraightened = {
      enable = mkEnableOption false;
      setDefault = mkOption {
        description = "If true, sets Nix Doom Unstraightened as default Emacs package.";
        type = types.bool;
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [
      inputs.emacs-overlay.overlays.default
      inputs.nix-doom-emacs-unstraightened.overlays.default
    ];

    services.emacs =
      {
        enable = true;
        install = true;
        defaultEditor = true;
      }
      // lib.optionalAttrs (!cfg.doomUnstraightened.setDefault) {
        package = normalPackage;
      };

    # emacs dependency
    modules.programs.aspell.enable = true;

    user.packages = let
      emacsPkg =
        if cfg.doomUnstraightened.setDefault
        then pkgs.emacsWithDoom
        else pkgs.doomEmacs;
    in
      userPackages
      ++ lib.optional cfg.doomUnstraightened.enable (unstraightenedPackage emacsPkg);

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
