local wezterm = require 'wezterm'

return {
  default_prog = { "/opt/homebrew/bin/nu" },
  keys = {
    {
        key = "p",
        mods = "CMD",
        action = wezterm.action.ActivateCommandPalette,
    },
    {
      key = "v",
      mods = "CMD|ALT",
      action = wezterm.action.SplitVertical {domain="CurrentPaneDomain"},
    },
    {
      key = "h",
      mods = "CMD|ALT",
      action = wezterm.action.SplitHorizontal {domain="CurrentPaneDomain"}
    },
  }
}
