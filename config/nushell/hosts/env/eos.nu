# NB temporary

$env.PATH = $env.PATH
    | split row (char esep)
    | prepend "/nix/var/nix/profiles/default/bin"
    | prepend "/etc/profiles/per-user/ryan/bin"
    | prepend "/run/current-system/sw/bin"
