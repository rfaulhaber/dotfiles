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

# `nix develop` and `nix-shell` overwrite SHELL with their own bash, and every
# process started from inside one inherits it — including `nix shell`, which
# execs $SHELL. Re-assert nushell on every startup so a leaked dev-shell SHELL
# cannot outlive the shell that leaked it. Hosts with no system profile
# (generated configs, non-Nix machines) fall back to this nushell's own path.
let system_nu = "/run/current-system/sw/bin/nu"
$env.SHELL = if ($system_nu | path exists) { $system_nu } else { $nu.current-exe }

# Doom Emacs bin
let emacs_bin_path = $'($env.HOME)/.emacs.d/bin'
let emacs_config_path = $'($env.HOME)/.config/emacs/bin'
let emacs_config_paths = [ $emacs_bin_path $emacs_config_path ]

if ([$emacs_bin_path $emacs_config_path] | path exists | any { |v| $v == true } ) {
    $env.PATH = ($env.PATH | split row (char esep) | prepend $emacs_config_paths)
}

# Generate the zoxide and carapace init scripts for standalone (non-home-manager)
# hosts. `source` is a parse-time keyword whose `def`/`alias` definitions only
# reach the interactive scope when the source statement is unnested, so the
# generated entry wrapper (nix/lib/configs/nushell.nix) sources these files at
# its top level and calls this to produce them. Home-manager hosts wire up
# zoxide/carapace through their own module and never call this. An empty stub
# keeps the wrapper's unconditional source parse-safe when a tool is absent.
def setup-shell-integrations [] {
  let zoxide_init = "~/.zoxide.nu"
  if (which zoxide | is-not-empty) {
    ^zoxide init nushell | save -f $zoxide_init
  } else {
    "" | save -f $zoxide_init
  }

  mkdir ~/.cache/carapace
  let carapace_init = "~/.cache/carapace/init.nu"
  if (which carapace | is-not-empty) {
    ^carapace _carapace nushell | save -f $carapace_init
  } else {
    "" | save -f $carapace_init
  }
}

# platform-specific env
match $nu.os-info.name {
      "macos" => { source "./hosts/env/darwin.nu" },
      "linux" => { source "./hosts/env/linux.nu" },
}

# hosts that get the shared server-style prompt (machines I ssh into)
let server_hosts = ["atlas" "vulcan" "hecate" "pallas" "janus" "prometheus"]
if (sys host | get hostname) in $server_hosts {
    source "./prompt.nu"
}

# host-specific env
match (sys host | get hostname) {
      "hyperion" => { source "./hosts/env/hyperion.nu" },
      "eos" => { source "./hosts/env/eos.nu" },
      "ponos" => { source "./hosts/env/ponos.nu" },
}

if ('/proc/version' | path exists) and (open '/proc/version' | find -i "microsoft" | length) > 0 {
  source "./hosts/env/wsl.nu"
}
