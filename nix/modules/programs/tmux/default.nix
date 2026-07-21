{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.tmux;
  colors = config.modules.themes.colors.withHashtag;
in {
  options.modules.programs.tmux = {
    enable = mkEnableOption false;
    keyMode = mkOption {
      description = "Key bindings for copy mode and the status line.";
      default = "vi";
      type = types.enum ["vi" "emacs"];
    };
    mouse = mkOption {
      description = "Enable mouse support (pane selection, resizing, scrolling).";
      default = true;
      type = types.bool;
    };
  };

  config = mkIf cfg.enable {
    home.programs.tmux = {
      enable = true;
      keyMode = cfg.keyMode;
      mouse = cfg.mouse;
      # Dev shells (direnv, `nix develop`) export SHELL as the store bash; a
      # tmux server started inside one would adopt it for every pane. Pin the
      # default shell rather than trusting the inherited environment.
      shell = mkIf config.modules.programs.nushell.enable "${pkgs.nushell}/bin/nu";
      terminal = "tmux-256color";
      baseIndex = 1;
      historyLimit = 50000;
      # tmux-sensible (sensibleOnTop) zeroes escape-time, which makes tmux
      # swallow legitimate ESC-prefixed sequences over ssh; 10ms is still
      # imperceptible for evil-mode ESC.
      escapeTime = 10;
      extraConfig = with colors; ''
        set -ga terminal-features ",*:RGB"

        set -g status-style "bg=${base01},fg=${base05}"
        set -g window-status-current-style "bg=${base02},fg=${base0D},bold"
        set -g pane-border-style "fg=${base02}"
        set -g pane-active-border-style "fg=${base0D}"
        set -g message-style "bg=${base01},fg=${base05}"
        set -g mode-style "bg=${base02},fg=${base05}"
      '';
    };
  };
}
