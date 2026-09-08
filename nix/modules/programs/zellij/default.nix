{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.zellij;
  colors = config.modules.themes.colors.withHashtag;

  # Must not collide with one of zellij's ~54 compiled-in theme names: on a
  # name clash the built-in silently wins over a theme defined in config.kdl,
  # so calling this "nord" or "tokyo-night" would leave the generated palette
  # inert with no error. Check `zellij setup --dump-config` before renaming.
  themeName = "base16";
  themeColors = import ./theme.nix {
    inherit colors;
    inherit (cfg) colorOverrides;
  };

  # Replaces zellij's default Ctrl-g, which is keyboard-quit in Emacs and
  # readline and the external-editor key in Claude Code. Unlike every
  # other zellij chord it cannot be escaped by locking, since it is the lock
  # toggle itself. Ctrl-; has no legacy control code, so nothing running in a
  # pane can claim it; it reaches zellij through the kitty keyboard protocol
  # (on by default in zellij, and spoken by ghostty). Under a terminal without
  # that protocol the key types a literal `;`; unlock with
  # `zellij action switch-mode normal` from inside the pane.
  lockKey = "Ctrl ;";
in {
  options.modules.programs.zellij = {
    enable = mkEnableOption false;
    colorOverrides = mkOption {
      description = ''
        Overrides for the semantic roles in `theme.nix` (e.g. `error`,
        `highlight`, `accent`), keyed by role with `#`-prefixed values. Use
        this when a base16 scheme's named slot doesn't match its label and the
        correction should stay local to zellij.
      '';
      default = {};
      type = types.attrsOf types.str;
    };
    defaultMode = mkOption {
      description = ''
        Input mode panes start in. `locked` keeps zellij's Ctrl-key bindings
        out of the way (useful under Emacs/readline) until unlocked with
        Ctrl-;.
      '';
      default = "normal";
      type = types.enum ["normal" "locked"];
    };
    mouse = mkOption {
      description = "Enable mouse support (pane focus, resizing, scrolling).";
      default = true;
      type = types.bool;
    };
    web = {
      enable = mkOption {
        description = ''
          Run a local web server alongside new sessions and let them be shared
          into it, reachable at `http://127.0.0.1:<port>/<session>`.

          Both underlying settings are read when a session is created, so
          already-running sessions keep whatever was in effect at their start —
          those can still be shared ad-hoc with `<SPACE>` in the share plugin.

          Access is gated on a login token (`zellij web --create-token`), not on
          this option.
        '';
        default = false;
        type = types.bool;
      };
      port = mkOption {
        description = "Port the web server listens on, bound to localhost.";
        default = 8082;
        type = types.port;
      };
    };
  };

  config = mkIf cfg.enable {
    home.programs.zellij = {
      enable = true;
      settings =
        {
          default_mode = cfg.defaultMode;
          keybinds = {
            # A top-level unbind strips the key from every mode, including the
            # defaults' `shared_except "locked"` block that holds the original
            # lock toggle. The replacement is bound in the same two places the
            # default was: in every unlocked mode to lock, and in locked to
            # unlock.
            unbind = {_args = ["Ctrl g"];};
            locked.bind = {
              _args = [lockKey];
              SwitchToMode = "normal";
            };
            shared_except = {
              _args = ["locked"];
              bind = {
                _args = [lockKey];
                SwitchToMode = "locked";
              };
            };
            # Renders as `bind "Alt y" { CopyLastCommandOutput }` in normal
            # mode. The action relies on OSC 133 prompt markers, which nushell
            # emits by default ($env.config.shell_integration.osc133); in a
            # shell without them the bind is a silent no-op.
            normal.bind = {
              _args = ["Alt y"];
              CopyLastCommandOutput = {};
            };
          };
          mouse_mode = cfg.mouse;
          theme = themeName;
          themes.${themeName} = themeColors;
        }
        # Dev shells (direnv, `nix develop`) export SHELL as the store bash;
        # zellij falls back to $SHELL when default_shell is unset. Pin the
        # default shell rather than trusting the inherited environment.
        // optionalAttrs config.modules.programs.nushell.enable {
          default_shell = "${pkgs.nushell}/bin/nu";
        }
        # `web_sharing` defaults to "off", which does not mean "never" — it
        # means each session must opt in individually. "on" makes sessions
        # shareable as soon as the server is up.
        // optionalAttrs cfg.web.enable {
          web_server = true;
          web_sharing = "on";
          web_server_port = cfg.web.port;
        };
    };
  };
}
