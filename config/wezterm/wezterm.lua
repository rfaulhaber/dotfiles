local wezterm = require 'wezterm'

local config = wezterm.config_builder()

-- deduce host information
local hostname = wezterm.hostname()
local host_info = {}

for v in string.gmatch(wezterm.target_triple, "[^-]+") do
  table.insert(host_info, v)
end

if #host_info == 3 then
  host_info.arch = host_info[1]
  host_info.vendor = host_info[2]
  host_info.os = host_info[3]
else
  host_info.arch = host_info[1]
  host_info.vendor = host_info[2] .. host_info[3]
  host_info.os = host_info[4]
end


-- host-specific config locations
local hosts = {
  hyperion = function()
    return require 'hyperion'
  end,
}

local platforms = {
  darwin = function()
    return require 'darwin'
  end,
  linux = function()
    return require 'linux'
  end,
};

-- platform-specific overrides
if host_info.os ~= null and platforms[host_info.os] ~= null then
  local overrides = platforms[host_info.os]()

  for k, v in pairs(overrides) do
    config[k] = v
  end
end

-- host-specific overrides
if hosts[hostname] ~= null then
  local overrides = hosts[hostname]()

  for k, v in pairs(overrides) do
    config[k] = v
  end
end

-- non-platform-specific config
config.font = wezterm.font 'Hack Nerd Font Mono'
config.color_scheme = 'Tokyo Night'

return config
