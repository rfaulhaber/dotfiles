#!/usr/bin/env nu

# Emit CSV (date,commit,lines) of total line counts in files matching a
# pathspec, sampled across git history. One commit per interval bucket
# (the last committed within that bucket).
#
# Examples:
#   ./bin/nix-lines-history.nu                         # daily, *.nix, stdout
#   ./bin/nix-lines-history.nu --interval week
#   ./bin/nix-lines-history.nu --pattern '*.org' -o org.csv

def main [
  --interval: string = "day"   # day | week | month
  --pattern: string = '*.nix'  # git pathspec to count
  --output (-o): path = ""     # file path; empty = stdout
] {
  let bucket_format = match $interval {
    "day"   => "%Y-%m-%d"
    "week"  => "%Y-W%V"
    "month" => "%Y-%m"
    _ => { error make { msg: $"unknown interval: ($interval)" } }
  }

  # Walk commits oldest-first; upsert by bucket so each bucket ends up
  # holding its latest commit.
  let sampled = (
    ^git log --format='%H|%cI' --reverse
    | lines
    | parse '{hash}|{date}'
    | insert bucket {|r| $r.date | into datetime | format date $bucket_format }
    | reduce --fold {} {|row, acc| $acc | upsert $row.bucket $row }
    | values
  )

  let rows = ($sampled | each {|c|
    # `^` matches every line, so `git grep -c '^'` yields `file:count`
    # rows; -I skips binary files. Empty result = no matching files.
    let res = (do { ^git grep -c -I '^' $c.hash -- $pattern } | complete)
    let total = if ($res.stdout | str trim | is-empty) { 0 } else {
      $res.stdout
      | lines
      | each {|l| $l | split row ':' | last | into int }
      | math sum
    }
    {
      date:   ($c.date | into datetime | format date '%Y-%m-%d')
      commit: ($c.hash | str substring 0..7)
      lines:  $total
    }
  })

  let csv = ($rows | to csv)
  if ($output | is-empty) { print $csv } else { $csv | save -f $output }
}
