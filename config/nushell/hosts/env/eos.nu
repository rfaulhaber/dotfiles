# NB temporary
# nix doesn't set the PATH correctly with nushell so we have to do it here
# this is really annoying!

$env.PATH = $env.PATH
    | split row (char esep)
    | prepend "/nix/var/nix/profiles/default/bin"
    | prepend "/etc/profiles/per-user/ryan/bin"
    | prepend "/run/current-system/sw/bin"

$env.SHELL = "/run/current-system/sw/bin/nu"
