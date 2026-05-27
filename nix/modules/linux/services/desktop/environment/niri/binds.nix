let
  spawn = args: {action.spawn = args;};
  action = name: {action.${name} = [];};
  actionVal = name: val: {action.${name} = val;};
in {
  "Mod+Shift+Slash" = action "show-hotkey-overlay";

  # Application launchers
  "Mod+Return" = spawn "ghostty";
  "Mod+D" = spawn "fuzzel";
  "Mod+Alt+D" = spawn ["fuzzel" "--list-executables-in-path"];
  "Super+Alt+L" = spawn "swaylock";
  "Mod+E" = spawn "~/.config/dotfiles/bin/exec-emacs-project.nu";

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

  # Focus navigation
  "Mod+Left" = action "focus-column-left";
  "Mod+Down" = action "focus-window-down";
  "Mod+Up" = action "focus-window-up";
  "Mod+Right" = action "focus-column-right";
  "Mod+H" = action "focus-column-left";
  "Mod+L" = action "focus-column-right";

  # Move columns/windows
  "Mod+Ctrl+Left" = action "move-column-left";
  "Mod+Ctrl+Down" = action "move-window-down";
  "Mod+Ctrl+Up" = action "move-window-up";
  "Mod+Ctrl+Right" = action "move-column-right";
  "Mod+Ctrl+H" = action "move-column-left";
  "Mod+Ctrl+L" = action "move-column-right";

  # Cross-workspace focus/move
  "Mod+J" = action "focus-window-or-workspace-down";
  "Mod+K" = action "focus-window-or-workspace-up";
  "Mod+Ctrl+J" = action "move-window-down-or-to-workspace-down";
  "Mod+Ctrl+K" = action "move-window-up-or-to-workspace-up";

  # Column first/last
  "Mod+Home" = action "focus-column-first";
  "Mod+End" = action "focus-column-last";
  "Mod+Ctrl+Home" = action "move-column-to-first";
  "Mod+Ctrl+End" = action "move-column-to-last";

  # Monitor focus/move
  "Mod+Alt+Left" = action "focus-monitor-left";
  "Mod+Alt+Down" = action "focus-monitor-down";
  "Mod+Alt+Up" = action "focus-monitor-up";
  "Mod+Alt+Right" = action "focus-monitor-right";
  "Mod+Alt+H" = action "focus-monitor-left";
  "Mod+Alt+J" = action "focus-monitor-down";
  "Mod+Alt+K" = action "focus-monitor-up";
  "Mod+Alt+L" = action "focus-monitor-right";

  "Mod+Alt+Ctrl+Left" = action "move-column-to-monitor-left";
  "Mod+Alt+Ctrl+Down" = action "move-column-to-monitor-down";
  "Mod+Alt+Ctrl+Up" = action "move-column-to-monitor-up";
  "Mod+Alt+Ctrl+Right" = action "move-column-to-monitor-right";
  "Mod+Alt+Ctrl+H" = action "move-column-to-monitor-left";
  "Mod+Alt+Ctrl+J" = action "move-column-to-monitor-down";
  "Mod+Alt+Ctrl+K" = action "move-column-to-monitor-up";
  "Mod+Alt+Ctrl+L" = action "move-column-to-monitor-right";

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
  "Mod+Alt+P" = action "screenshot-window";
  "Mod+Alt+W" = action "screenshot-screen";

  # Misc
  "Mod+Escape" = {
    allow-inhibiting = false;
    action.toggle-keyboard-shortcuts-inhibit = [];
  };
  "Mod+Shift+E" = action "quit";
  "Mod+Shift+P" = action "power-off-monitors";
  "Mod+A" = action "toggle-overview";
}
