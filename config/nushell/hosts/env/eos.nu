# NB temporary

$env.PATH = $env.PATH
    | split row (char esep)
    | prepend "/nix/var/nix/profiles/default/bin"
    | prepend "/run/current-system/sw/bin"
