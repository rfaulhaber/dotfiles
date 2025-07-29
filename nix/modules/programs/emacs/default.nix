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
}:
with lib; let
  cfg = config.modules.programs.emacs;
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
  userPackages = with pkgs; [
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
    inputs.nil.outputs.packages.${pkgs.stdenv.targetPlatform.system}.nil
    languagetool
    nodePackages.mermaid-cli
    ripgrep
    sqlite
    texlive.combined.scheme-medium
    wordnet
    zstd
  ];

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

    # TODO temporary, not sustainable, but necessary because determinate nix doesn't correctly build nix.conf on macOS
    # TODO write to an emacs.conf file, include it in nix.custom.conf
    environment.etc."nix/nix.custom.conf".text = mkIf isDarwin ''
      extra-substituters = https://nix-community.cachix.org
      extra-trusted-public-keys = nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=
    '';

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

    # TODO handle better
    environment.etc."xdg/mimeapps.list".text = mkIf isLinux ''
      [Default Applications]
      application/pdf=emacs.desktop
    '';

    home.programs.nushell.shellAliases =
      mkIf config.modules.programs.nushell.enable shellAliases
      // lib.optionalAttrs isDarwin {
        emacs = "${pkgs.emacs}/Applications/Emacs.app/Contents/MacOS/Emacs";
      };

    home.file.doomconfig = mkIf (!cfg.doomUnstraightened.enable) {
      source = config.dotfiles.emacsDir;
      target = "${config.user.home}/.config/doom";
    };
  };
}
