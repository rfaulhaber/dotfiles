export def prepend-paths [...paths: string]: list<string> -> list<string> {
    $env.PATH
        | split row (char esep)
        | prepend $paths
}

export def append-paths [...paths: string]: list<string> -> list<string> {
    $env.PATH
        | split row (char esep)
        | append $paths
}

export def is-wsl [] -> bool {
  ('/proc/version' | path exists) and (open '/proc/version' | find -i "microsoft" | length) > 0
}
