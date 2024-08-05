#!/usr/bin/env nu

def main [query?: string] {
    let key = (pass wallpaper)
    let base_url = $"https://api.unsplash.com/photos/random/?client_id=($key)&orientation=landscape"

    let url = if query == null { $base_url } else { $"($base_url)&query=($query)" }

    let log_file = "~/.wallpaper-log.json" | path expand

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

    open $log_file | append $log_record | to json | save -f $log_file

    feh --bg-fill $filename
}
