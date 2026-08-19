export use ./colored_man_pages.nu
export use ./net.nu
export use ./nix.nu
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

  $in | str lowercase | split chars | each {
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

# Converts a dotenv file into a Nushell record, with the env variable names as keys.
export def "read-dotenv" [
  file?: path # If a raw string is not passed into this command, you must specify a file to open and read
]: [
  string -> record
  nothing -> record
] {
    let input = if $in == null {
      open $file
    } else {
      $in
    }

    $input
        | lines
        | where { |line| ($line | str trim) != "" and (not ($line | str starts-with '#')) }
        | split column --number 2 '='
        | rename left right
        | reduce --fold {} { |row, acc| $acc | merge { $row.left: $row.right } }
}

# TODO find ports used in a set of yaml files
# export def "compose-ports" [
#   paths: glob
# ]: nothing -> list<int> {

# }
