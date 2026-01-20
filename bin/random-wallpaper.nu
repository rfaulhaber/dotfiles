#!/usr/bin/env nu

const log_file_path = "~/.local/share/random-wallpaper/log.json"

def main [--token: string, --token-file: string, --desktop: string, --monitor: string, query?: string] {
    let key = if $token_file != null {
      open $token_file
    } else if $token != null {
      $token
    } else {
      $in
    }

    if ($key | is-empty) {
      print -e "No token value passed in."
      exit 1
    }

    let base_url = $"https://api.unsplash.com/photos/random/?client_id=($key)&orientation=landscape"

    let url = if query == null { $base_url } else { $"($base_url)&query=($query)" }

    let log_file = $log_file_path | path expand
    let log_file_exists = $log_file | path exists

    if not $log_file_exists {
      mkdir ($log_file | path dirname)
      "{}" | save -f $log_file
    }

    let tmpdir = (mktemp -d)

    let res = (http get $url)

    if ("errors" in res)  {
       print -e "Unsplash reported errors. Aborting."
       print -e $"Error: (res | get errors)"

       exit 1
    }

    let filename = $"($tmpdir)/($res | get id).jpg"

    http get ($res | get urls.full) | save -f $filename

    let log_record = $res
        | select id urls.full description alt_description links.html
        | rename -c {urls.full: download_url}
        | rename -c {links.html: html_link}
        | update description { |r| if $r.description == null { $r.alt_description } else { $r.description } }
        | reject alt_description

    open $log_file
        | append $log_record
        | to json
        | save -f $log_file

    match $desktop {
        "wayland" => {
            # TODO handle multiple displays when it becomes relevant
            ^swww img $filename
        },
        "xserver" => {
            ^feh --bg-fill $filename
        }
    }
}
