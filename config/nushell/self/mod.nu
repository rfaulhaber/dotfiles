export use ./colored_man_pages.nu
export use ./util.nu

# Runs ^tokei and returns the output as a Nu record.
export def tokei-summary [] {
  let output = (^tokei --output json | from json)
  let total = ($output | get Total.code)

  let langs = ($output | columns | drop)

  let lang_stats = ($langs | each { |lang_name|
    let lang = ($output | get $lang_name)

    let code = ($lang | get code)
    let comments = ($lang | get comments)
    let blanks = ($lang | get blanks)
    let lines = $code + $comments + $blanks

    {
        language: $lang_name,
        files: ($lang | get reports | length),
        lines: $lines,
        code: $code,
        comments: $comments,
        blanks: $blanks,
        percentage: ($code / $total),
    }
  } );

  $lang_stats
}

# Export English dictionary from ^aspell
export def words [] {
  if (which aspell | length) < 1 {
     error make {msg: "Aspell is not present in the PATH." help: "Install Aspell and/or add it to the PATH."}
  }

  ^aspell -d en dump master
  | ^aspell -l en expand
  | lines
}

# Returns a random element from a list.
export def "get random" []: list<any> -> any {
  let count = ($in | length)

  let idx = (..($count - 1) | collect | shuffle | first)

  $in | get $idx
}

# Rotates chars in string by n.
export def rotn [n: int]: string -> string {
  let min = 'a' | into binary | into int
  let max = ('z' | into binary | into int) + 1

  $in | str downcase | split chars | each {
    into binary
    | into int
    | do {
        let c = ($in + $n)
            if $c > $max {
                $min + $c mod $max
            } else {
                $c
            }
        }
    | char -i $in
  }
    | str join
}

# wrapper for creating nix shells with unfree software
export def "nix shell-unfree" [flake: string] {
  with-env { NIXPKGS_ALLOW_UNFREE: 1 } { ^nix shell --impure $flake }
}

export def "nix run-unfree" [flake: string]: nothing -> nothing {
  with-env { NIXPKGS_ALLOW_UNFREE: 1 } { ^nix run --impure $flake }
}

export def "nix-diff" [before: path, after: path]: nothing -> string  {
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
