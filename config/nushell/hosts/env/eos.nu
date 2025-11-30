# NB temporary
# nix doesn't set the PATH correctly with nushell so we have to do it here
# this is really annoying!

$env.PATH = $env.PATH
    | split row (char esep)
    | prepend "/nix/var/nix/profiles/default/bin"
    | prepend "/etc/profiles/per-user/ryan/bin"
    | prepend "/run/current-system/sw/bin"

$env.SHELL = "/run/current-system/sw/bin/nu"

let home_dir = $env | get --optional HOME | default "/Users/ryan"
let wezterm_config_dir = $"($home_dir)/.config/wezterm"


# these aren't set automatically on my macbook for some reason...
$env.WEZTERM_CONFIG_DIR = $wezterm_config_dir
$env.WEZTERM_CONFIG_FILE = $"($wezterm_config_dir)/wezterm.lua"
