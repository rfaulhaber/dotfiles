#!/usr/bin/env nu

const projects_dir = "~/Projects"

def main []: nothing -> nothing {
  let projects = $projects_dir
    | path expand
    | ls $in
    | get name
    | each { path basename }
    | uniq
    | sort
    | to text --no-newline

  let fuzzel_input = $projects | ^fuzzel --dmenu --prompt "Projects: " | default ""

  if $fuzzel_input != "" {
    let full_project_path = $"~/Projects/($fuzzel_input)" | path expand
    ^emacs --chdir $full_project_path
  }
}
