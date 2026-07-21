# wrapper for creating nix shells with unfree software
export def "shell-unfree" [flake: string] {
  with-env { NIXPKGS_ALLOW_UNFREE: 1 } { ^nix shell --impure $flake }
}

# wrapper for running nix programs with unfree software
export def "run-unfree" [flake: string]: nothing -> nothing {
  with-env { NIXPKGS_ALLOW_UNFREE: 1 } { ^nix run --impure $flake }
}

# enter a flake dev shell in nushell instead of the bash `nix develop` hardwires
export def --wrapped dev [...args: string] {
  ^nix develop ...$args --command nu
}

export def "diff-metadata" [before: path, after: path]: nothing -> string  {
  nix store diff-closures $before $after --json
  | from json
  | get packages
  | transpose package version
  | each { |r|
    let package_name = $r.package
    let version_info = $r.version

    let before = $version_info | get versionsBefore | if ($in | is-empty) { "<none>" } else { $in | first }
    let after = $version_info | get versionsAfter | if ($in | is-empty) { "<none>" } else { $in | first }

    $"($package_name)\t\t($before)->($after)"
  }
  | str join "\n"
}
