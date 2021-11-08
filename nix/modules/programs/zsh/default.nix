{ config, lib, pkgs, platform, ... }:

with lib;

let
  cfg = config.modules.programs.zsh;
  colors = config.modules.themes.colors;
in {
  options.modules.programs.zsh = {
    enable = mkEnableOption false;
    setDefault = mkOption {
      description = "Sets Zsh to be default shell for system user.";
      default = false;
      type = types.bool;
    };
    theme = mkOption {
      description = "Zsh theme from oh-my-zsh.";
      default = "agnoster";
      type = types.str;
    };
    # TODO make shell agnostic
    useZoxide = mkOption {
      description = "Use Zoxide with Zsh.";
      # I use Zoxide by default
      default = true;
      type = types.bool;
    };
  };
  config = mkIf cfg.enable {
    environment = {
      systemPackages = with pkgs; [
        nix-zsh-completions
        xclip # required for pbcopy and pbpaste
      ];

      # sometimes zsh from nixpkgs doesn't respect highlightStyle value
      variables = { ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE = "fg=${colors.grey}"; };
    };

    home.programs = {
      zsh = {
        enable = true;
        enableAutosuggestions = true;
        enableCompletion = true;
        enableSyntaxHighlighting = true;

        oh-my-zsh = {
          enable = true;
          plugins = [ "git" "colored-man-pages" ];
          theme = cfg.theme;
        };

        shellAliases = {
          pbcopy = "xclip -selection clipboard";
          pbpaste = "xclip -selection clipboard -o";
          ls = "exa";
          l = "exa -lah";
          ll = "exa -lh";
          ec = "emacsclient";
          eo = "emacsclient -n"; # "emacs open"
        };
      };

      zoxide.enable = (mkIf cfg.useZoxide) true;
    };

    user.shell = mkIf cfg.setDefault pkgs.zsh;

    # required by home-manager zsh
    environment.pathsToLink = [ "/share/zsh" ];
  };
}
