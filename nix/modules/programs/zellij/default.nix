{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.zellij;
  colors = config.modules.themes.colors.withHashtag;
in {
  options.modules.programs.zellij = {
    enable = mkEnableOption false;
    defaultMode = mkOption {
      description = ''
        Input mode panes start in. `locked` keeps zellij's Ctrl-key bindings
        out of the way (useful under Emacs/readline) until unlocked with
        Ctrl-g.
      '';
      default = "normal";
      type = types.enum ["normal" "locked"];
    };
    mouse = mkOption {
      description = "Enable mouse support (pane focus, resizing, scrolling).";
      default = true;
      type = types.bool;
    };
  };

  config = mkIf cfg.enable {
    home.programs.zellij = {
      enable = true;
      settings =
        {
          default_mode = cfg.defaultMode;
          mouse_mode = cfg.mouse;
          theme = "base16";
          # Semantic aliases rather than raw baseXX slots, so per-theme custom
          # files and host-level `themes.overrides` corrections apply here too.
          themes.base16 = {
            inherit (colors) fg bg red green yellow blue magenta cyan orange;
            black = colors."bg-alt";
            white = colors."bright-white";
          };
        }
        # Dev shells (direnv, `nix develop`) export SHELL as the store bash;
        # zellij falls back to $SHELL when default_shell is unset. Pin the
        # default shell rather than trusting the inherited environment.
        // optionalAttrs config.modules.programs.nushell.enable {
          default_shell = "${pkgs.nushell}/bin/nu";
        };
    };
  };
}
