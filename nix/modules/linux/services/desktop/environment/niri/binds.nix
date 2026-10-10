{
  config,
  lib,
  pkgs,
}: let
  spawn = args: {action.spawn = args;};
  action = name: {action.${name} = [];};
  actionVal = name: val: {action.${name} = val;};
  terminal = "ghostty";
  mkNuScript = name:
    builtins.readFile "${config.dotfiles.binDir}/${name}.nu"
    |> lib.my.writeNushellScriptBin pkgs name;

  # Package the launcher script as its own content-addressed derivation rather
  # than spawning it from the ~/.config/dotfiles mirror, so the bind depends
  # only on the script's contents (reproducible) and not on a symlinked source
  # tree whose store path varies by flake fetch method.
  execEmacsProject = mkNuScript "exec-emacs-project";
  openZellijWorkspace = mkNuScript "open-zellij-workspace";
  launchExecutable = mkNuScript "launch-executable";

  # fcitx5 binds the same chord, but compositor binds win over the input
  # method's keyboard grab. Owning it here lets the switch poke the bar widget,
  # which would otherwise only notice at its next poll.
  toggleInputMethod = pkgs.writeShellScript "toggle-input-method" ''
    fcitx5-remote -t
    noctalia msg plugin ryan/ime:poll all refresh >/dev/null 2>&1 || true
  '';
in {
  "Mod+Shift+Slash" = action "show-hotkey-overlay";

  # Application launchers
  "Mod+Return" = spawn [terminal "--command='zellij'"];
  "Mod+S" = spawn "${openZellijWorkspace}/bin/open-zellij-workspace";
  "Mod+D" = spawn ["noctalia" "msg" "panel-toggle" "launcher"];
  "Mod+Shift+D" = spawn "${launchExecutable}/bin/launch-executable";
  "Mod+E" = spawn "${execEmacsProject}/bin/exec-emacs-project";

  # Volume control
  "XF86AudioRaiseVolume" = {
    allow-when-locked = true;
    action.spawn = ["wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1+"];
  };
  "XF86AudioLowerVolume" = {
    allow-when-locked = true;
    action.spawn = ["wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1-"];
  };
  "XF86AudioMute" = {
    allow-when-locked = true;
    action.spawn = ["wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle"];
  };
  "XF86AudioMicMute" = {
    allow-when-locked = true;
    action.spawn = ["wpctl" "set-mute" "@DEFAULT_AUDIO_SOURCE@" "toggle"];
  };

  # Window management
  "Mod+W" = action "close-window";

  # Focus navigation; horizontal moves continue onto the adjacent monitor at
  # the edge, the way J/K continue onto the adjacent workspace.
  "Mod+Left" = action "focus-column-or-monitor-left";
  "Mod+Down" = action "focus-window-down";
  "Mod+Up" = action "focus-window-up";
  "Mod+Right" = action "focus-column-or-monitor-right";
  "Mod+H" = action "focus-column-or-monitor-left";
  "Mod+L" = action "focus-column-or-monitor-right";

  # Move columns/windows
  "Mod+Ctrl+Left" = action "move-column-left-or-to-monitor-left";
  "Mod+Ctrl+Down" = action "move-window-down";
  "Mod+Ctrl+Up" = action "move-window-up";
  "Mod+Ctrl+Right" = action "move-column-right-or-to-monitor-right";
  "Mod+Ctrl+H" = action "move-column-left-or-to-monitor-left";
  "Mod+Ctrl+L" = action "move-column-right-or-to-monitor-right";

  # Cross-workspace focus/move
  "Mod+J" = action "focus-window-or-workspace-down";
  "Mod+K" = action "focus-window-or-workspace-up";
  "Mod+Ctrl+J" = action "move-window-down-or-to-workspace-down";
  "Mod+Ctrl+K" = action "move-window-up-or-to-workspace-up";

  # Column first/last
  "Mod+Shift+H" = action "focus-column-first";
  "Mod+Shift+L" = action "focus-column-last";
  "Mod+Ctrl+Shift+H" = action "move-column-to-first";
  "Mod+Ctrl+Shift+L" = action "move-column-to-last";

  # Monitor focus/move. With two outputs "next" is a toggle, so no direction
  # to pick and one press regardless of how many columns are open.
  "Mod+O" = action "focus-monitor-next";
  "Mod+Ctrl+O" = action "move-column-to-monitor-next";
  "Mod+Shift+O" = action "move-workspace-to-monitor-next";

  # Workspace navigation
  "Mod+Page_Down" = action "focus-workspace-down";
  "Mod+Page_Up" = action "focus-workspace-up";
  "Mod+U" = action "focus-workspace-down";
  "Mod+I" = action "focus-workspace-up";
  "Mod+Ctrl+Page_Down" = action "move-column-to-workspace-down";
  "Mod+Ctrl+Page_Up" = action "move-column-to-workspace-up";
  "Mod+Ctrl+U" = action "move-column-to-workspace-down";
  "Mod+Ctrl+I" = action "move-column-to-workspace-up";

  "Mod+Shift+Page_Down" = action "move-workspace-down";
  "Mod+Shift+Page_Up" = action "move-workspace-up";
  "Mod+Shift+U" = action "move-workspace-down";
  "Mod+Shift+I" = action "move-workspace-up";

  # Mouse wheel workspace switching
  "Mod+WheelScrollDown" = {
    cooldown-ms = 150;
    action.focus-workspace-down = [];
  };
  "Mod+WheelScrollUp" = {
    cooldown-ms = 150;
    action.focus-workspace-up = [];
  };
  "Mod+Ctrl+WheelScrollDown" = {
    cooldown-ms = 150;
    action.move-column-to-workspace-down = [];
  };
  "Mod+Ctrl+WheelScrollUp" = {
    cooldown-ms = 150;
    action.move-column-to-workspace-up = [];
  };

  # Mouse wheel column switching
  "Mod+WheelScrollRight" = action "focus-column-right";
  "Mod+WheelScrollLeft" = action "focus-column-left";
  "Mod+Ctrl+WheelScrollRight" = action "move-column-right";
  "Mod+Ctrl+WheelScrollLeft" = action "move-column-left";

  # Shift+wheel horizontal scrolling emulation
  "Mod+Shift+WheelScrollDown" = action "focus-column-right";
  "Mod+Shift+WheelScrollUp" = action "focus-column-left";
  "Mod+Ctrl+Shift+WheelScrollDown" = action "move-column-right";
  "Mod+Ctrl+Shift+WheelScrollUp" = action "move-column-left";

  # Workspace by index
  "Mod+1" = actionVal "focus-workspace" 1;
  "Mod+2" = actionVal "focus-workspace" 2;
  "Mod+3" = actionVal "focus-workspace" 3;
  "Mod+4" = actionVal "focus-workspace" 4;
  "Mod+5" = actionVal "focus-workspace" 5;
  "Mod+6" = actionVal "focus-workspace" 6;
  "Mod+7" = actionVal "focus-workspace" 7;
  "Mod+8" = actionVal "focus-workspace" 8;
  "Mod+9" = actionVal "focus-workspace" 9;
  "Mod+Ctrl+1" = actionVal "move-column-to-workspace" 1;
  "Mod+Ctrl+2" = actionVal "move-column-to-workspace" 2;
  "Mod+Ctrl+3" = actionVal "move-column-to-workspace" 3;
  "Mod+Ctrl+4" = actionVal "move-column-to-workspace" 4;
  "Mod+Ctrl+5" = actionVal "move-column-to-workspace" 5;
  "Mod+Ctrl+6" = actionVal "move-column-to-workspace" 6;
  "Mod+Ctrl+7" = actionVal "move-column-to-workspace" 7;
  "Mod+Ctrl+8" = actionVal "move-column-to-workspace" 8;
  "Mod+Ctrl+9" = actionVal "move-column-to-workspace" 9;

  # Column consume/expel
  "Mod+BracketLeft" = action "consume-or-expel-window-left";
  "Mod+BracketRight" = action "consume-or-expel-window-right";
  "Mod+Comma" = action "consume-window-into-column";
  "Mod+Period" = action "expel-window-from-column";

  # Window sizing
  "Mod+R" = action "switch-preset-column-width";
  "Mod+Shift+R" = action "switch-preset-window-height";
  "Mod+Ctrl+R" = action "reset-window-height";
  "Mod+F" = action "maximize-column";
  "Mod+Shift+F" = action "fullscreen-window";
  "Mod+Ctrl+F" = action "expand-column-to-available-width";
  "Mod+C" = action "center-column";

  "Mod+Minus" = actionVal "set-column-width" "-10%";
  "Mod+Equal" = actionVal "set-column-width" "+10%";
  "Mod+Shift+Minus" = actionVal "set-window-height" "-10%";
  "Mod+Shift+Equal" = actionVal "set-window-height" "+10%";

  # Floating/tiling
  "Mod+V" = action "toggle-window-floating";
  "Mod+Shift+V" = action "switch-focus-between-floating-and-tiling";

  # Tabbed display
  "Mod+T" = action "toggle-column-tabbed-display";

  # Screenshots
  "Mod+P" = action "screenshot";
  "Mod+Shift+P" = action "screenshot-window";
  "Mod+Ctrl+P" = action "screenshot-screen";

  # Misc
  "Mod+Escape" = {
    allow-inhibiting = false;
    action.toggle-keyboard-shortcuts-inhibit = [];
  };
  "Mod+Shift+E" = action "quit";
  # Mod and Alt share the left thumb on the Moonlander, so Mod+Alt is kept for
  # binds that should be hard to hit by accident.
  "Mod+Alt+P" = action "power-off-monitors";
  "Mod+Shift+S" = spawn ["noctalia" "msg" "session" "lock"];
  "Mod+Space" = lib.mkIf config.modules.desktop.inputMethod.enable (spawn "${toggleInputMethod}");
  "Mod+A" = action "toggle-overview";
}
