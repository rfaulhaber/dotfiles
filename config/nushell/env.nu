# Nushell Environment Config File

const dotfiles_path = "~/Projects/dotfiles/config/nushell"

# Directories to search for scripts when calling source or use
$env.NU_LIB_DIRS = [
    ($nu.default-config-dir | path join "hosts")
    ($nu.default-config-dir | path join "hosts/env")
    ($nu.default-config-dir | path join "hosts/config")
    ($nu.default-config-dir | path join "themes")
    $dotfiles_path
]

# Pager
$env.PAGER = "bat -p"
$env.MANPAGER = "sh -c 'col -bx | bat -l man -p'"
$env.MANROFFOPT = "-c"

# Doom Emacs bin
let emacs_bin_path = $'($env.HOME)/.emacs.d/bin'
let emacs_config_path = $'($env.HOME)/.config/emacs/bin'
let emacs_config_paths = [ $emacs_bin_path $emacs_config_path ]

if ([$emacs_bin_path $emacs_config_path] | path exists | any { |v| $v == true } ) {
    $env.PATH = ($env.PATH | split row (char esep) | prepend $emacs_config_paths)
}

# Host-specific env
match $nu.os-info.name {
      "macos" => { source "./hosts/env/darwin.nu" },
      "linux" => { source "./hosts/env/linux.nu" },
}

# Hosts that get the shared server-style prompt (machines I ssh into).
let server_hosts = ["atlas" "vulcan" "hecate" "pallas" "janus"]
if (sys host | get hostname) in $server_hosts {
    source "./prompt.nu"
}

match (sys host | get hostname) {
      "hyperion" => { source "./hosts/env/hyperion.nu" },
      "eos" => { source "./hosts/env/eos.nu" },
      "ponos" => { source "./hosts/env/ponos.nu" },
}

if ('/proc/version' | path exists) and (open '/proc/version' | find -i "microsoft" | length) > 0 {
  source "./hosts/env/wsl.nu"
}
