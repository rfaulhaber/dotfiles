{ config, lib, pkgs, platform, ... }:

with lib;

let
  cfg = config.modules.programs.zsh;
  colors = config.modules.themes.colors;
in {
  options.modules.programs.zsh = {
    enable = mkEnableOption false;
    setDefault = mkOption {
      description = "Sets ZSH to be default shell for system user.";
      default = false;
      type = types.bool;
    };
    theme = mkOption {
      description = "ZSH theme from oh-my-zsh.";
      default = "agnoster";
      type = types.str;
    };
  };
  config = mkIf cfg.enable {
    environment = {
      systemPackages = with pkgs; [
        nix-zsh-completions
        oh-my-zsh
        xclip # required for pbcopy and pbpaste
        zsh
        zsh-autosuggestions
        zsh-completions
      ];

      # sometimes zsh from nixpkgs doesn't respect highlightStyle value
      variables = { ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE = "fg=${colors.grey}"; };
    };

    programs.zsh = {
      enable = true;
      ohMyZsh = {
        enable = true;
        plugins = [ "git" "colored-man-pages" ];
        theme = cfg.theme;
      };
      autosuggestions.enable = true;
      autosuggestions.highlightStyle = "fg=${colors.grey}";
      syntaxHighlighting.enable = true;
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

    user.shell = mkIf cfg.setDefault pkgs.zsh;
  };
}
