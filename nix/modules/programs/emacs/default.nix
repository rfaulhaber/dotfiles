# this module is not very, well, /modular/. it's pretty specific to my extremely
# specific emacs configuration I would like to change that!
{
  config,
  lib,
  pkgs,
  inputs,
  isLinux,
  isDarwin,
  ...
}: let
  inherit (lib) mkEnableOption mkOption types mkIf mkMerge optionals optionalAttrs;
  cfg = config.modules.programs.emacs;
  shellAliases = {
    ec = "emacsclient";
    eo = "emacsclient -n"; # "emacs open"
  };
  emacsPackages = epkgs: [
    epkgs.pdf-tools
    epkgs.prettier
    epkgs.vterm
    epkgs.tree-sitter
    epkgs.tree-sitter-langs
    epkgs.treesit-grammars.with-all-grammars
  ];

  # dependencies for my very specific configuration of doom
  # see doom.d/init.el for more
  # we need to include every program either directly or indirectly referenced in config
  userPackages =
    [
      pkgs.alejandra
      pkgs.ast-grep
      pkgs.clang # unfortunately we need a C compiler for various dependencies
      pkgs.cmake
      pkgs.direnv
      pkgs.djvulibre
      pkgs.editorconfig-core-c
      pkgs.fd
      pkgs.fzf
      pkgs.git
      pkgs.gnumake
      pkgs.gnutls
      pkgs.graphviz
      pkgs.imagemagick
      inputs.nil.outputs.packages.${pkgs.stdenv.targetPlatform.system}.nil
      pkgs.languagetool
      pkgs.nodePackages.mermaid-cli
      pkgs.ripgrep
      pkgs.sqlite
      pkgs.texlive.combined.scheme-medium
      pkgs.wordnet
      pkgs.zstd
    ]
    ++ optionals isDarwin [
      # emacs can't use nushell ls and macOS ls doesn't work right with dired
      pkgs.uutils-coreutils-noprefix
    ];

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

  resolvedEmacsPkg = let
    emacsPkg =
      if cfg.doomUnstraightened.setDefault
      then pkgs.emacsWithDoom
      else pkgs.doomEmacs;
  in (unstraightenedPackage emacsPkg);
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
    nix.settings = {
      substituters = [
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };

    nixpkgs.overlays = [
      inputs.emacs-overlay.overlays.default
      inputs.nix-doom-emacs-unstraightened.overlays.default
    ];

    services.emacs =
      {
        enable = true;
        package =
          if cfg.doomUnstraightened.setDefault
          then resolvedEmacsPkg
          else normalPackage;
      }
      // lib.optionalAttrs isLinux {
        defaultEditor = true;
        install = true;
      };

    # emacs dependency
    modules.programs.aspell.enable = true;

    user.packages =
      userPackages
      ++ lib.optional cfg.doomUnstraightened.enable resolvedEmacsPkg;

    home.programs.nushell.shellAliases =
      mkIf config.modules.programs.nushell.enable shellAliases;

    home.file.doomconfig = mkIf (!cfg.doomUnstraightened.enable) {
      source = config.dotfiles.emacsDir;
      target = "${config.user.home}/.config/doom";
    };

    home.file.doomApp = mkIf isDarwin {
      source = "${config.services.emacs.package}/Applications/Emacs.app";
      target = "${config.user.home}/Applications/Emacs.app";
    };
  };
}
