{ config, lib, pkgs, platform, ... }:

with lib;

let
  cfg = config.common.programs.zsh;
  colors = config.common.themes.colors;
in {
  options.common.programs.zsh = {
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
    useDirenv = mkOption {
      description = "Use Zoxide with Zsh.";
      # I use Direnv by default
      default = true;
      type = types.bool;
    };
  };
  config = mkIf cfg.enable {
    environment = {
      systemPackages = with pkgs; [
        nix-zsh-completions
        (mkIf pkgs.stdenv.isLinux xclip) # required for pbcopy and pbpaste
        (mkIf cfg.useZoxide zoxide)
        (mkIf cfg.useDirenv direnv)
      ];

      # sometimes zsh from nixpkgs doesn't respect highlightStyle value
      variables = { ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE = "fg=${colors.grey}"; };
    };

    programs.zsh.enable = mkIf pkgs.stdenv.isDarwin true;

    home.programs.zsh = {
      enable = true;
      oh-my-zsh = {
        enable = true;
        plugins = [ "git" "colored-man-pages" ];
        theme = cfg.theme;
      };
      enableAutosuggestions = true;
      enableCompletion = true;
      enableSyntaxHighlighting = true;
      initExtraFirst = let
        zoxideInit = ''
          eval "$(${pkgs.zoxide}/bin/zoxide init zsh)"
        '';
        # we have to use the plain executable here because for some reason the
        # pkgs.direnv version doesn't have permission to run
        direnvInit = ''
          eval "$(direnv hook zsh)"
        '';
      in ''
        ${if cfg.useZoxide then zoxideInit else ""}
        ${if cfg.useDirenv then direnvInit else ""}
        ${if config.common.programs.emacs.enable then
          "PATH=$PATH:~/.emacs.d/bin"
        else
          ""}
      '';
      shellAliases = {
        pbcopy = mkIf pkgs.stdenv.isLinux "xclip -selection clipboard";
        pbpaste = mkIf pkgs.stdenv.isLinux "xclip -selection clipboard -o";
      };
    };

    user.shell = mkIf cfg.setDefault pkgs.zsh;

    environment.pathsToLink = [ "/share/zsh" ];
  };
}
