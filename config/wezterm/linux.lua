local wezterm = require 'wezterm'

return {
  leader = {
    key = 'a', mods = 'CTRL'
  }
  keys = {
    {
      key = '|',
      mods = 'LEADER',
      action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
    },
    {
      key = '-',
      mods = 'LEADER',
      action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
    },
    {
      key = 'c',
      mods = 'LEADER',
      action = wezterm.action.ActivateCommandPalette,
    },
    {
      key = 'p',
      mods = 'LEADER',
      action = wezterm.action.PaneSelect,
    }
  }
}
