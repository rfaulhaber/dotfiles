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
  }
}
