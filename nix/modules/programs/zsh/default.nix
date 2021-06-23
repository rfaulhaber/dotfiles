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
  };
  config = mkIf cfg.enable {
    environment = {
      systemPackages = with pkgs; [
        oh-my-zsh
        zsh
        zsh-autosuggestions
        zsh-completions
        xclip
        nix-zsh-completions
      ];

      # sometimes zsh from nixpkgs doesn't respect highlightStyle value
      variables = { ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE = "fg=${colors.grey}"; };
    };

    programs.zsh = {
      enable = true;
      ohMyZsh = {
        enable = true;
        plugins = [ "git" "colored-man-pages" ];
        theme = "agnoster";
      };
      autosuggestions.enable = true;
      autosuggestions.highlightStyle = "fg=${colors.grey}";
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
