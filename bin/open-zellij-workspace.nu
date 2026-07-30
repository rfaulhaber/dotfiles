#!/usr/bin/env nu

const projects_dir = "~/Projects"

def main []: nothing -> nothing {
  let projects = $projects_dir
    | path expand
    | ls $in
    | where type == "dir"
    | get name
    | each { path basename }
    | uniq
    | sort
    | to text --no-newline

  let fuzzel_input = $projects | ^fuzzel --dmenu --prompt "Projects: " | default ""

  if $fuzzel_input != "" {
    let full_project_path = [$projects_dir $fuzzel_input] | path join | path expand
    ^ghostty $"--working-directory=($full_project_path)" -e zellij attach --create $fuzzel_input
  }
}
