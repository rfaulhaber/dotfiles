# Emacs built around my Doom configuration via nix-doom-emacs-unstraightened.
# The daemon package is always `emacsWithDoom`: it provides bin/emacs +
# bin/emacsclient (and, on Darwin, Emacs.app) with the Doom profile baked in,
# so the service and emacsclient see Doom directly. Works on NixOS and nix-darwin.
{
  config,
  lib,
  pkgs,
  inputs,
  isLinux,
  isDarwin,
  ...
}: let
  inherit (lib) mkEnableOption mkOption types mkIf optionals optionalAttrs;
  cfg = config.modules.programs.emacs;

  shellAliases = {
    ec = "emacsclient";
    eo = "emacsclient -n"; # "emacs open"
  };

  # Elisp packages Doom expects Nix to supply rather than build via straight.el.
  emacsPackages = epkgs:
    with epkgs;
      [
        pdf-tools
        prettier
        tree-sitter
        tree-sitter-langs
        treesit-grammars.with-all-grammars
      ]
      ++ lib.optionals config.modules.programs.ghostty.enable [ghostel evil-ghostel];

  # Programs referenced (directly or indirectly) by doom.d; see doom.d/init.el.
  userPackages = with pkgs;
    [
      alejandra
      ast-grep
      clang # several packages need a C compiler available
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
      inputs.nil.packages.${pkgs.stdenv.targetPlatform.system}.default
      languagetool
      mermaid-cli
      pandoc
      ripgrep
      sqlite
      texliveMedium
      wordnet
      zstd
    ]
    ++ optionals isDarwin [
      # emacs can't use nushell ls, and macOS ls misbehaves with dired
      pkgs.uutils-coreutils-noprefix
    ];

  emacsWithDoom = pkgs.emacsWithDoom {
    emacs = cfg.package;
    # Must be a path literal so Nix copies doom.d into the store with proper
    # context; dotfiles.emacsDir is a toString'd path that loses store context.
    doomDir = ../../../../doom.d;
    doomLocalDir = "${config.user.home}/.local/share/nix-doom";
    extraPackages = emacsPackages;
    extraBinPackages = userPackages;
    experimentalFetchTree = true;
  };
in {
  options.modules.programs.emacs = {
    enable = mkEnableOption "Emacs with my Doom configuration";
    package = mkOption {
      description = "Base Emacs package Doom is built on top of.";
      type = types.package;
      default = pkgs.emacs;
    };
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays =
      [
        inputs.emacs-overlay.overlays.default
        inputs.nix-doom-emacs-unstraightened.overlays.default
      ]
      ++ lib.optionals config.modules.programs.ghostty.enable [
        inputs.ghostel.overlays.default
        # unstraightened does not use this pkgs' emacsPackagesFor: it rebuilds
        # the package set via its own emacs-overlay pin, whose trailing
        # `esuper.override` re-applies makeOverridable args but drops
        # overrideScope layers — including ghostel-flake's. Re-inject the
        # flake's ghostel through the manualPackages *arg* (merged last in
        # nixpkgs' emacs-packages.nix), which survives that rebuild, so Doom's
        # pinned ghostel grafts the flake's native module instead of nixpkgs'
        # older one (which fails ghostel--minimum-module-version at runtime).
        (_final: prev: {
          emacsPackagesFor = emacs:
            (prev.emacsPackagesFor emacs).overrideScope (
              _eself: esuper:
                esuper.override (args: {
                  manualPackages =
                    (args.manualPackages or esuper.manualPackages)
                    // {inherit (esuper) ghostel;};
                })
            );
        })
      ];

    services.emacs =
      {
        enable = true;
        package = emacsWithDoom;
      }
      // optionalAttrs isLinux {
        defaultEditor = true;
        install = true;
      };

    # spell-checking backend for doom's :checkers spell
    modules.programs.aspell.enable = true;

    user.packages = userPackages ++ [emacsWithDoom];

    home.programs.nushell.shellAliases =
      mkIf config.modules.programs.nushell.enable shellAliases;

    home.file.doomApp = mkIf isDarwin {
      source = "${config.services.emacs.package}/Applications/Emacs.app";
      target = "${config.user.home}/Applications/Emacs.app";
    };
  };
}
