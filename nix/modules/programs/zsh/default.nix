{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.programs.zsh;
in {
  options.modules.programs.zsh = {
    enable = mkEnableOption false;
    setDefault = mkOption {
      description = "Sets ZSH to be default shell for system user.";
      default = false;
      type = types.bool;
    };
  };
  config = mkIf cfg.enable {
    environment = {
      systemPackages = with pkgs; [
        oh-my-zsh
        zsh
        zsh-autosuggestions
        zsh-completions
      ];

      variables = { ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE = "fg=#41505E"; };
    };

    programs.zsh = {
      enable = true;
      ohMyZsh = {
        enable = true;
        plugins = [ "git" "colored-man-pages" ];
        theme = "agnoster";
      };
      autosuggestions.enable = true;
      autosuggestions.highlightStyle = "fg=#41505E";
      syntaxHighlighting.enable = true;
      shellAliases = {
        pbcopy = "xclip -selection clipboard";
        pbpaste = "xclip -selection clipboard -o";
        vi = "nvim";
        vim = "nvim";
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
