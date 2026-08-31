#!/usr/bin/env nu

def main []: nothing -> nothing {
  # Basenames only: dedupes shadowed copies across profile dirs, and spawning
  # by name re-resolves through the same PATH order the shell would use.
  let executables = $env.PATH
    | where { $in | path exists }
    | each { try { ls --long $in | where type in ["file" "symlink"] | where mode =~ "x" | get name } catch { [] } }
    | flatten
    | each { path basename }
    | uniq
    | sort
    | to text --no-newline

  let selection = $executables | ^noctalia dmenu --prompt "Run: " | default ""

  if $selection != "" {
    ^$selection
  }
}
