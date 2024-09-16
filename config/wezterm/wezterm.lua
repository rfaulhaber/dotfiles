-- Pull in the wezterm API
local wezterm = require 'wezterm'

local hostname = wezterm.hostname()

local config = wezterm.config_builder()

-- host-specific config locations
local hosts = {
  hyperion = function()
    return require 'hosts.hyperion'
  end
}

-- host-specific overrides
if hosts[hostname] ~= null then
  local overrides = hosts[hostname]()

  for k, v in pairs(overrides) do
    config[k] = v
  end
end

config.font = wezterm.font 'Hack Nerd Font Mono'

config.color_scheme = 'Tokyo Night'

return config
