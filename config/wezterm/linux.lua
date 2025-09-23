local wezterm = require 'wezterm'

return {
  leader = {
    key = 'a', mods = 'CTRL'
  }
  keys = {
    {
        key = "p",
        mods = "SUPER",
        action = wezterm.action.ActivateCommandPalette,
    },
    {
      key = "v",
      mods = "SUPER|ALT",
      action = wezterm.action.SplitVertical {domain="CurrentPaneDomain"},
    },
    {
      key = "h",
      mods = "SUPER|ALT",
      action = wezterm.action.SplitHorizontal {domain="CurrentPaneDomain"}
    },
    {
        key = "t",
        mods = "SUPER|SHIFT",
        action = wezterm.action.SpawnCommandInNewTab {cwd = wezterm.home_dir, domain = "CurrentPaneDomain"}
    },
    {
        key = "n",
        mods = "SUPER|SHIFT",
        action = wezterm.action.SpawnCommandInNewWindow {cwd = wezterm.home_dir, domain = "CurrentPaneDomain"}
    }
  }
}
