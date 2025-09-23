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
        action = wezterm.action.SpawnTab {cwd = wezterm.home_dir}
    },
    {
        key = "n",
        mods = "SUPER|SHIFT",
        action = wezterm.action.SpawnWindow {cwd = wezterm.home_dir}
    }
  }
}
