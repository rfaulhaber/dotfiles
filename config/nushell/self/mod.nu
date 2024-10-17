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
        percentage: ($lines / $total),
    }
  } );

  $lang_stats
}

# Export English dictionary from ^aspell
export def words [] {
  ^aspell -d en dump master
  | ^aspell -l en expand
  | lines
}
