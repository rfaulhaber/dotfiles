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

  let selection = $projects | ^noctalia dmenu --prompt "Projects: " | default ""

  if $selection != "" {
    let full_project_path = [$projects_dir $selection] | path join | path expand
    ^ghostty $"--working-directory=($full_project_path)" -e zellij attach --create $selection
  }
}
