#!/usr/bin/env nu

const log_file_path = "~/.local/share/random-wallpaper/log.json"

# Enumerate connected outputs for the given desktop. Returns a list of strings:
# - wayland: output names (e.g. "DP-1", "DP-3") via `awww query --json`
# - xserver: monitor names via `xrandr --listactivemonitors`
# - darwin:  desktop indices ("1", "2", ...) — macOS addresses displays by index
def get_outputs [desktop: string]: nothing -> list<string> {
    match $desktop {
        "wayland" => {
            # `awww query --json` emits an object keyed by daemon namespace,
            # each holding one record per output:
            #   {"": [{"name": "DP-1", "width": 3840, ...}]}
            # The default namespace is the empty string; collect across every
            # key so a custom `awww-daemon --namespace` still resolves. awww
            # prints nothing at all when it knows of no outputs, which
            # `from json` would reject — hence the guard.
            let raw = (^awww query --json | str trim)

            if ($raw | is-empty) {
                []
            } else {
                $raw | from json | values | flatten | get name
            }
        },
        "xserver" => {
            ^xrandr --listactivemonitors
                | lines
                | skip 1
                | each {|l| $l | split row " " | last | str trim }
                | where {|n| ($n | str length) > 0 }
        },
        "darwin" => {
            let count = (
                ^osascript -e 'tell application "System Events" to count desktops'
                | str trim
                | into int
            )
            1..$count | each {|i| $i | into string }
        },
        _ => []
    }
}

# Apply a single wallpaper to all displays for the given desktop. Used both
# for the non-perDisplay case and as the fallback when output enumeration
# fails under --per-display.
def set_single [desktop: string, filename: string] {
    match $desktop {
        "wayland" => { ^awww img $filename },
        "xserver" => { ^feh --bg-fill $filename },
        "darwin" => {
            let script = $'tell application "System Events" to tell every desktop to set picture to POSIX file "($filename)"'
            ^osascript -e $script
        }
    }
}

# Fetch one wallpaper from Unsplash, save it to $tmpdir, append a log record,
# and return the path to the downloaded file.
def fetch_wallpaper [key: string, query: string, tmpdir: string, log_file: string]: nothing -> string {
    let base_url = $"https://api.unsplash.com/photos/random/?client_id=($key)&orientation=landscape"
    let url = if ($query | is-empty) { $base_url } else { $"($base_url)&query=($query)" }

    let res = (http get $url)

    if ("errors" in $res) {
       print -e "Unsplash reported errors. Aborting."
       print -e $"Error: ($res | get errors)"
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

    $filename
}

def main [
    --token: string,
    --token-file: string,
    --desktop: string,
    --per-display,        # fetch and set a distinct wallpaper for each connected display
    query?: string
] {
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

    let log_file = $log_file_path | path expand
    let log_file_exists = $log_file | path exists

    if not $log_file_exists {
      mkdir ($log_file | path dirname)
      "[]" | save -f $log_file
    }

    let tmpdir = (mktemp -d)
    let q = if $query == null { "" } else { $query }

    if not $per_display {
        let filename = (fetch_wallpaper $key $q $tmpdir $log_file)
        set_single $desktop $filename
    } else {
        let outputs = (get_outputs $desktop)

        if ($outputs | is-empty) {
            print -e $"Could not enumerate outputs for desktop '($desktop)'; falling back to single wallpaper."
            let filename = (fetch_wallpaper $key $q $tmpdir $log_file)
            set_single $desktop $filename
            return
        }

        match $desktop {
            "wayland" => {
                for $output in $outputs {
                    let filename = (fetch_wallpaper $key $q $tmpdir $log_file)
                    ^awww img --outputs $output $filename
                }
            },
            "xserver" => {
                # feh assigns positional images to monitors in order, so we
                # collect all files first and pass them in one invocation.
                let files = $outputs | each {|_| fetch_wallpaper $key $q $tmpdir $log_file }
                ^feh --bg-fill ...$files
            },
            "darwin" => {
                for $i in $outputs {
                    let filename = (fetch_wallpaper $key $q $tmpdir $log_file)
                    let script = $'tell application "System Events" to tell desktop ($i) to set picture to POSIX file "($filename)"'
                    ^osascript -e $script
                }
            }
        }
    }
}
