#!/usr/bin/env nu

const log_file_path = "~/.local/share/random-wallpaper/log.json"

# Downloads kept in --store-dir beyond whatever is on screen. Two outputs on a
# 30 minute timer turn this over in a few hours, which is enough history for
# noctalia's picker panel to browse back through.
const store_keep = 12

# Enumerate connected outputs for the given desktop. Returns a list of strings:
# - wayland:  output names (e.g. "DP-1", "DP-3") via `awww query --json`
# - noctalia: the same connector names, via niri
# - xserver:  monitor names via `xrandr --listactivemonitors`
# - darwin:   desktop indices ("1", "2", ...) — macOS addresses displays by index
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
        "noctalia" => {
            # noctalia has no IPC verb that lists outputs, and wallpaper-set
            # rejects any connector the compositor is not currently driving,
            # so ask niri. Outputs disabled in niri's config are still listed
            # there, just without a logical geometry.
            ^niri msg --json outputs
                | from json
                | values
                | where {|o| $o.logical? != null }
                | get name
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
        # Without a connector noctalia updates every output plus its default
        # entry in one batch.
        "noctalia" => { ^noctalia msg wallpaper-set $filename },
        "xserver" => { ^feh --bg-fill $filename },
        "darwin" => {
            let script = $'tell application "System Events" to tell every desktop to set picture to POSIX file "($filename)"'
            ^osascript -e $script
        }
    }
}

# Apply a wallpaper to one display only. `output` is whatever get_outputs
# returned for the desktop.
def set_output [desktop: string, output: string, filename: string] {
    match $desktop {
        "wayland" => { ^awww img --outputs $output $filename },
        "noctalia" => { ^noctalia msg wallpaper-set $output $filename },
        "darwin" => {
            let script = $'tell application "System Events" to tell desktop ($output) to set picture to POSIX file "($filename)"'
            ^osascript -e $script
        },
        _ => {
            print -e $"Desktop '($desktop)' cannot address a single display."
            exit 1
        }
    }
}

# noctalia's unit is Type=simple, so at login this can run before its socket
# answers. wallpaper-get is a cheap, side-effect-free probe.
def wait_for_noctalia [] {
    for _attempt in 1..30 {
        let probe = (do { ^noctalia msg wallpaper-get } | complete)
        if $probe.exit_code == 0 { return }
        sleep 1sec
    }
    print -e "noctalia did not answer over IPC within 30s."
    exit 1
}

# noctalia re-reads its persisted wallpaper paths at startup, so anything on
# screen has to survive; beyond that keep only the newest $store_keep files.
def prune_store [store_dir: string, outputs: list<string>] {
    let on_screen = (
        ([null] ++ $outputs)
        | each {|output|
            let probe = (
                if $output == null {
                    do { ^noctalia msg wallpaper-get } | complete
                } else {
                    do { ^noctalia msg wallpaper-get $output } | complete
                }
            )
            if $probe.exit_code == 0 { $probe.stdout | str trim | path basename } else { null }
        }
        | compact
    )

    ls $store_dir
        | where type == file
        | sort-by modified --reverse
        | skip $store_keep
        | where {|f| ($f.name | path basename) not-in $on_screen }
        | each {|f| rm $f.name }
        | ignore
}

# Fetch one wallpaper from Unsplash, save it to $dir, append a log record,
# and return the path to the downloaded file.
def fetch_wallpaper [key: string, query: string, dir: string, log_file: string]: nothing -> string {
    let base_url = $"https://api.unsplash.com/photos/random/?client_id=($key)&orientation=landscape"
    let url = if ($query | is-empty) { $base_url } else { $"($base_url)&query=($query)" }

    let res = (http get $url)

    if ("errors" in $res) {
       print -e "Unsplash reported errors. Aborting."
       print -e $"Error: ($res | get errors)"
       exit 1
    }

    let filename = $"($dir)/($res | get id).jpg"

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
    --output: string,     # fetch and set a wallpaper for this one display only
    --store-dir: string,  # keep downloads here instead of a throwaway temp dir
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

    let dir = if $store_dir == null {
        mktemp -d
    } else {
        let expanded = ($store_dir | path expand)
        mkdir $expanded
        $expanded
    }
    let q = if $query == null { "" } else { $query }

    if $desktop == "noctalia" {
        wait_for_noctalia
    }

    if $output != null {
        let filename = (fetch_wallpaper $key $q $dir $log_file)
        set_output $desktop $output $filename
    } else if not $per_display {
        let filename = (fetch_wallpaper $key $q $dir $log_file)
        set_single $desktop $filename
    } else {
        let outputs = (get_outputs $desktop)

        if ($outputs | is-empty) {
            print -e $"Could not enumerate outputs for desktop '($desktop)'; falling back to single wallpaper."
            let filename = (fetch_wallpaper $key $q $dir $log_file)
            set_single $desktop $filename
        } else if $desktop == "xserver" {
            # feh assigns positional images to monitors in order, so we
            # collect all files first and pass them in one invocation.
            let files = $outputs | each {|_| fetch_wallpaper $key $q $dir $log_file }
            ^feh --bg-fill ...$files
        } else {
            for $o in $outputs {
                let filename = (fetch_wallpaper $key $q $dir $log_file)
                set_output $desktop $o $filename
            }
        }
    }

    if $desktop == "noctalia" and $store_dir != null {
        prune_store $dir (get_outputs $desktop)
    }
}
