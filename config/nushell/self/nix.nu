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

# list root flake inputs whose upstream has moved past flake.lock, without touching it
export def "flake-outdated" [...inputs: string]: nothing -> table {
  let tmp = (mktemp -t flake.lock.XXXXXX)
  # --refresh bypasses tarball-ttl, which otherwise serves a branch's cached rev for an hour
  ^nix flake update ...$inputs --refresh --output-lock-file $tmp
  let old = (open --raw flake.lock | from json)
  let new = (open --raw $tmp | from json)
  rm $tmp

  $old.nodes.root.inputs | transpose input key | each { |r|
    let o = ($old.nodes | get $r.key | get locked)
    let n = ($new.nodes | get ($new.nodes.root.inputs | get $r.input) | get locked)
    if $o.narHash != $n.narHash {
      {
        input: $r.input
        locked: ($o.lastModified * 1_000_000_000 | into datetime)
        latest: ($n.lastModified * 1_000_000_000 | into datetime)
        previous_rev: ($o.rev? | default "" | str substring 0..6)
        latest_rev: ($n.rev? | default "" | str substring 0..6)
      }
    }
  }
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
