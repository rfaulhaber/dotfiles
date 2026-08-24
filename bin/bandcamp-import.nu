#!/usr/bin/env nu

# Import a batch of Bandcamp purchases into the atlas music library.
#
# Bandcamp hands you one .zip per album. This unzips each, reads the embedded
# tags to derive Artist / Album / Year, previews the plan, and on confirmation
# rsyncs every album into /data/music/<Artist>/<Album>/ on atlas — the shared
# directory Navidrome streams from and Lidarr manages.
#
# Hybrid registration: after the files land, it asks Lidarr (over its API) to
# look the artist up in MusicBrainz. On a match the artist is added as an
# UNMONITORED catalog entry (we already own the music — monitoring would send
# Lidarr off downloading "missing" albums) and RefreshArtist links the files in
# place. On no match — common for Bandcamp-exclusive / self-released artists —
# the files are still on disk and Navidrome serves them; only the Lidarr catalog
# entry is skipped. Either way the music plays.
#
# Lidarr lives behind gluetun on atlas:8686. Its published podman port is
# reachable on the LAN (DNAT happens before the nixos firewall), so the API
# calls go straight to http://atlas:8686 — this keeps artist names with quotes
# (Guns N' Roses) out of any remote shell command line. The API key is read once
# from atlas's config.xml over ssh. A preflight disables the Lidarr branch
# cleanly if the API can't be reached.
#
# Usage:
#   nu bin/bandcamp-import.nu ~/Downloads/bandcamp           # transfer + register
#   nu bin/bandcamp-import.nu ~/Downloads/bandcamp --dry-run # preview only
#   nu bin/bandcamp-import.nu ~/dl --no-lidarr               # Navidrome only
#   nu bin/bandcamp-import.nu ~/dl -y                        # skip confirmation
#
# Batch mode (`batch` subcommand) is the non-interactive flavor of the same
# pipeline, meant to run ON atlas under the bandcamp-import systemd path unit
# (modules.services.bandcamp-import). It drains a drop directory instead of
# taking a one-off folder: zips arrive in <watch>/incoming, albums are placed
# locally (no ssh), the Lidarr key is read from a local path, and every zip
# ends up in <watch>/archive or <watch>/failed. See "main batch" at the bottom
# for the path-unit contract.
#   nu bin/bandcamp-import.nu batch /data/import/bandcamp
#
# Requires locally: unzip, rsync, ffprobe (ffmpeg). If missing:
#   nix shell nixpkgs#unzip nixpkgs#rsync nixpkgs#ffmpeg
#
# Debug logging (opt-in, written to stderr so it never pollutes the result
# table on stdout): set BANDCAMP_DEBUG to 1/true/yes/on, e.g.
#   BANDCAMP_DEBUG=1 nu bin/bandcamp-import.nu ~/dl --dry-run
# The API key is never logged — only its source path and length.

const AUDIO_EXTS = [flac mp3 m4a ogg opus wav aiff aif alac wv]

def debug-on []: nothing -> bool {
  ($env.BANDCAMP_DEBUG? | default "" | str lowercase) in ["1" "true" "yes" "on"]
}

# Diagnostic line on stderr, gated by BANDCAMP_DEBUG. stdout stays reserved for
# the result table so the script composes in a pipeline.
def dbg [msg: string] {
  if (debug-on) { print -e $"(ansi dark_gray)[debug] ($msg)(ansi reset)" }
}

# Bandcamp filenames sanitize cleanly, but tags can carry a stray slash that
# would otherwise fork the path. Only '/' is illegal in a path component on
# Linux; leave everything else intact so titles round-trip faithfully.
def sanitize [s: string]: nothing -> string {
  $s | str replace -a "/" "-" | str trim
}

# Case-insensitive tag lookup: ffprobe normalizes FLAC vorbis comments to lower
# case but ID3/MP4 atoms can surface as Artist/ARTIST. Returns the first present
# name's value, else null.
def tag-get [tags: record, names: list<string>]: nothing -> any {
  for n in $names {
    let hit = ($tags | columns | where {|c| ($c | str lowercase) == ($n | str lowercase) })
    if ($hit | is-not-empty) {
      return ($tags | get ($hit | first))
    }
  }
  null
}

# Tags sharpen artist/year (e.g. album_artist on compilations) but aren't
# required — Bandcamp's "Artist - Album" naming carries the fallback. Returns {}
# when ffprobe is absent so the caller degrades to filename parsing.
def ffprobe-tags [file: string]: nothing -> record {
  if (which ffprobe | is-empty) {
    dbg "ffprobe absent — using filename parsing"
    return {}
  }
  let res = (try { ^ffprobe -v quiet -print_format json -show_format $file | complete } catch { null })
  if $res == null or $res.exit_code != 0 {
    dbg $"ffprobe failed on ($file | path basename) (exit ($res.exit_code? | default 'n/a'))"
    return {}
  }
  let tags = (try { $res.stdout | from json | get format.tags? | default {} } catch { {} })
  dbg $"ffprobe tags for ($file | path basename): [($tags | columns | str join ', ')]"
  $tags
}

# List a literal directory's immediate children. Album titles contain glob
# metacharacters — "( )", "[Deluxe]" — so we glob a relative pattern from inside
# the dir (scoped cd) rather than feeding the metacharacter-bearing path to glob.
def kids [dir: string]: nothing -> list<string> {
  do { cd $dir; glob "*" }
}

# Strip Bandcamp's single wrapper folder (a zip sometimes extracts to
# "<stem>/Artist - Album/..."). Descend through any directory that holds exactly
# one subdir and no files of its own, so the rsync source is the track folder.
def content-root [dir: string]: nothing -> string {
  let children = (kids $dir)
  let files = ($children | where {|p| ($p | path type) == "file" })
  let subdirs = ($children | where {|p| ($p | path type) == "dir" })
  if ($files | is-empty) and (($subdirs | length) == 1) {
    dbg $"descending wrapper folder ($subdirs | first | path basename)"
    content-root ($subdirs | first)
  } else {
    $dir
  }
}

def du-bytes [dir: string]: nothing -> int {
  let r = (^du -sb $dir | complete)
  if $r.exit_code != 0 { return 0 }
  try { $r.stdout | split row "\t" | first | into int } catch { 0 }
}

def four-digit-year [s: string]: nothing -> string {
  let m = ($s | parse -r '(?<y>\d{4})')
  if ($m | is-empty) { "" } else { $m | first | get y }
}

# Read one extracted album dir into a plan row. Prefers tags; falls back to
# Bandcamp's "Artist - Album" folder naming when tags are thin.
def describe-album [dir: string]: nothing -> any {
  let root = (content-root $dir)
  let audio = (
    do { cd $root; glob "**/*" --no-dir }
    | where {|p| ($p | path parse | get extension | str lowercase) in $AUDIO_EXTS }
  )
  if ($audio | is-empty) { return null }

  let tags = (ffprobe-tags ($audio | first))
  let stem = ($dir | path basename)
  let parts = ($stem | split row " - ")

  let artist = (
    tag-get $tags [album_artist albumartist artist album-artist]
    | default (if (($parts | length) > 1) { $parts | first } else { $stem })
  )
  let album = (
    tag-get $tags [album]
    | default (if (($parts | length) > 1) { $parts | skip 1 | str join " - " } else { "Unknown Album" })
  )
  let year = (four-digit-year (tag-get $tags [date year originalyear] | default ""))
  let bytes = (du-bytes $root)
  let source = (if ($tags | is-empty) { "filename" } else { "tags" })
  dbg $"parsed ($dir | path basename): artist='($artist)' album='($album)' year='($year)' tracks=($audio | length) via ($source)"

  {
    artist: (sanitize ($artist | into string))
    album: (sanitize ($album | into string))
    year: $year
    tracks: ($audio | length)
    size: $bytes
    src: $root
  }
}

# --- Lidarr API (called from this host against http://atlas:8686) -------------

def parse-api-key [xml: string]: nothing -> any {
  let m = ($xml | parse -r '<ApiKey>(?<k>[^<]+)</ApiKey>')
  if ($m | is-empty) { null } else { $m | first | get k }
}

def lidarr-key [host: string]: nothing -> any {
  let src = "/data/apps/lidarr/config.xml"
  let res = (^ssh -o ConnectTimeout=10 $host $"cat ($src)" | complete)
  dbg $"ssh ($host) cat ($src) -> exit ($res.exit_code)"
  if $res.exit_code != 0 {
    dbg $"ssh stderr: ($res.stderr | str trim)"
    return null
  }
  let k = (parse-api-key $res.stdout)
  if $k == null {
    dbg "no <ApiKey> element found in config.xml"
    null
  } else {
    # Never log the key itself — only enough to confirm it was read.
    dbg $"API key read from ($src): length ($k | str length)"
    $k
  }
}

def lidarr-get [base: string, key: string, path: string]: nothing -> any {
  dbg $"GET ($base)/api/v1($path)"
  http get --headers ["X-Api-Key" $key] $"($base)/api/v1($path)"
}

def lidarr-post [base: string, key: string, path: string, body: any]: nothing -> any {
  dbg $"POST ($base)/api/v1($path)"
  http post --content-type application/json --headers ["X-Api-Key" $key] $"($base)/api/v1($path)" $body
}

# Resolve the root folder / profile ids the add-artist call needs. Null on any
# API failure so callers degrade to Navidrome-only placement.
def lidarr-context [url: string, key: string]: nothing -> any {
  try {
    let ctx = {
      key: $key
      root: (lidarr-get $url $key "/rootfolder" | first | get path)
      qid: (lidarr-get $url $key "/qualityprofile" | first | get id)
      mid: (lidarr-get $url $key "/metadataprofile" | first | get id)
    }
    dbg $"Lidarr context: root ($ctx.root), quality ($ctx.qid), metadata ($ctx.mid)"
    $ctx
  } catch {|e|
    dbg $"Lidarr preflight error: ($e.msg)"
    null
  }
}

# Ensure the artist exists in Lidarr's catalog, then trigger a disk scan that
# links the just-placed files. Returns a status record; never throws.
def lidarr-register [base: string, key: string, root: string, qid: int, mid: int, name: string]: nothing -> record {
  try {
    let lookup = (lidarr-get $base $key $"/artist/lookup?(({term: $name} | url build-query))")
    dbg $"lookup '($name)' -> ($lookup | length) candidate\(s\)"
    if ($lookup | is-empty) { return {status: "no-match", detail: $name} }

    let cand = ($lookup | first)
    let fid = $cand.foreignArtistId
    dbg $"top candidate: '($cand.artistName)' mbid ($fid)"
    let existing = (lidarr-get $base $key "/artist" | where foreignArtistId == $fid)

    let artist = (if ($existing | is-not-empty) {
      dbg $"already in Lidarr as artist id ($existing | first | get id)"
      $existing | first
    } else {
      let body = ($cand | merge {
        qualityProfileId: $qid
        metadataProfileId: $mid
        rootFolderPath: $root
        monitored: false
        addOptions: { monitor: "none", searchForMissingAlbums: false }
      })
      dbg $"adding artist '($cand.artistName)' (root ($root), quality ($qid), metadata ($mid))"
      lidarr-post $base $key "/artist" $body
    })

    dbg $"RefreshArtist artist id ($artist.id)"
    lidarr-post $base $key "/command" { name: "RefreshArtist", artistId: $artist.id } | ignore
    {
      status: (if ($existing | is-not-empty) { "exists" } else { "added" })
      detail: $artist.artistName
    }
  } catch {|e|
    dbg $"lidarr-register error: ($e.msg)"
    { status: "error", detail: ($e.msg | str substring 0..80) }
  }
}

def main [
  input_dir: path = "."        # directory of Bandcamp .zip files
  --host: string = "atlas"     # ssh target (also the Lidarr host)
  --lidarr-url: string = "http://atlas:8686"
  --music-root: string = "/data/music"
  --dry-run                    # preview the plan, transfer nothing
  --no-lidarr                  # skip Lidarr; just place files for Navidrome
  --yes (-y)                   # skip the confirmation prompt
] {
  let zips = (glob $"($input_dir)/*.zip")
  dbg $"found ($zips | length) zip\(s\) in ($input_dir)"
  if ($zips | is-empty) {
    print -e $"(ansi yellow)No .zip files found in ($input_dir).(ansi reset)"
    exit 1
  }

  if (which ffprobe | is-empty) {
    print -e $"(ansi yellow)ffprobe not found — parsing Artist/Album from filenames. For richer tags: nix shell nixpkgs#ffmpeg(ansi reset)"
  }

  let work = (mktemp -d -t bandcamp.XXXXXX)
  dbg $"extracting to ($work)"
  let albums = (
    $zips
    | each {|zip|
        let dest = ($work | path join ($zip | path basename | str replace -r '(?i)\.zip$' ''))
        mkdir $dest
        ^unzip -q -o $zip -d $dest
        let row = (describe-album $dest)
        if $row == null {
          print -e $"(ansi yellow)⚠ no audio in ($zip | path basename) — skipping.(ansi reset)"
          null
        } else {
          $row | insert target $"($music_root)/($row.artist)/($row.album)"
        }
      }
    | where {|r| $r != null }
  )

  if ($albums | is-empty) {
    rm -rf $work
    print -e $"(ansi yellow)Nothing to import.(ansi reset)"
    exit 1
  }

  print ($albums | select artist album year tracks size target | update size {|r| $r.size | into filesize })

  if $dry_run {
    rm -rf $work
    print $"(ansi blue)Dry run — ($albums | length) album\(s\) would transfer to ($host).(ansi reset)"
    exit 0
  }

  if not $yes {
    # input needs a TTY; a non-interactive stdin means "no confirmation given".
    let resp = (try { input $"Trrespfer ($albums | length) album\(s\) to ($host)? [y/N] " } catch { "" })
    if ($resp | str lowercase) not-in ["y" "yes"] {
      rm -rf $work
      print "Aborted (use -y to skip confirmation)."
      exit 0
    }
  }

  # Resolve Lidarr context once. Any failure here demotes the whole run to
  # Navidrome-only rather than erroring per album.
  let lidarr = (if $no_lidarr {
    null
  } else {
    let key = (lidarr-key $host)
    if $key == null {
      print -e $"(ansi yellow)⚠ couldn't read Lidarr API key from ($host); registering nothing.(ansi reset)"
      null
    } else {
      let ctx = (lidarr-context $lidarr_url $key)
      if $ctx == null {
        print -e $"(ansi yellow)⚠ Lidarr API at ($lidarr_url) unreachable; registering nothing.(ansi reset)"
      }
      $ctx
    }
  })

  let results = (
    $albums
    | each {|a|
        print $"(ansi cyan)→ ($a.artist) — ($a.album)(ansi reset)"
        dbg $"rsync ($a.src)/ -> ($host):($a.target)/"
        # -s/--protect-args keeps spaces in titles intact across the remote
        # shell; --mkpath creates the Artist/Album parents on atlas. Capture the
        # exit code (keeping live progress on the terminal, hence no pipe) so the
        # result reflects what actually transferred.
        let transferred = (try {
          ^rsync -a -s --mkpath --info=progress2 $"($a.src)/" $"($host):($a.target)/"
          $env.LAST_EXIT_CODE == 0
        } catch {
          false
        })
        if not $transferred {
          print -e $"(ansi red)✗ rsync failed for ($a.artist) — ($a.album); skipping Lidarr.(ansi reset)"
        }

        # Registering an artist whose files didn't land would tell Lidarr to scan
        # an empty folder, so gate it on a successful transfer.
        let lid = (if not $transferred {
          "skipped"
        } else if $lidarr == null {
          "skipped"
        } else {
          let r = (lidarr-register $lidarr_url $lidarr.key $lidarr.root $lidarr.qid $lidarr.mid $a.artist)
          $r.status
        })
        { artist: $a.artist, album: $a.album, transferred: $transferred, lidarr: $lid }
      }
  )

  rm -rf $work

  print ($results)
  let ok = ($results | where transferred)
  let failed = ($results | where not transferred)
  let unmatched = ($results | where lidarr == "no-match")
  if ($unmatched | is-not-empty) {
    print $"(ansi yellow)($unmatched | length) artist\(s\) not in MusicBrainz — placed for Navidrome, not catalogued in Lidarr.(ansi reset)"
  }
  print $"(ansi green)✓ ($ok | length) album\(s\) imported to ($host).(ansi reset)"
  if ($failed | is-not-empty) {
    print -e $"(ansi red)✗ ($failed | length) album\(s\) failed to transfer.(ansi reset)"
    exit 1
  }
}

# --- batch mode (systemd service on atlas) ------------------------------------
# Output here is deliberately plain — it lands in the journal, where ansi
# escapes render as literal garbage.

# Collisions (a re-dropped zip) get a timestamp prefix so an earlier archived
# or quarantined copy is never clobbered.
def unique-dest [dir: string, base: string]: nothing -> string {
  let cand = ($dir | path join $base)
  if not ($cand | path exists) {
    $cand
  } else {
    $dir | path join $"(date now | format date '%Y%m%d-%H%M%S')-($base)"
  }
}

def quarantine [zip_path: string, failed_dir: string, reason: string] {
  print -e $"✗ ($zip_path | path basename): ($reason) — quarantined in ($failed_dir)"
  mv $zip_path (unique-dest $failed_dir ($zip_path | path basename))
}

# Import a single settled zip end to end. Every path out of here moves the zip
# from incoming/ into archive/ or failed/ — leaving one behind would re-match
# the path unit's glob on exit and re-trigger the service in a tight loop.
def import-one [
  zip_path: string
  work: string
  failed_dir: string
  archive_dir: string
  music_root: string
  lidarr_url: string
  lidarr: any
]: nothing -> record {
  let name = ($zip_path | path basename)
  let skipped = { zip: $name, artist: null, album: null, imported: false, lidarr: "skipped" }

  # A truncated upload eventually settles (its mtime stops moving) but fails
  # the integrity test; quarantining it here is what terminates the retry loop.
  let test = (^unzip -qq -t $zip_path | complete)
  if $test.exit_code != 0 {
    quarantine $zip_path $failed_dir "corrupt or truncated zip"
    return $skipped
  }

  let dest = ($work | path join ($name | str replace -r '(?i)\.zip$' ''))
  mkdir $dest
  let ext = (^unzip -q -o $zip_path -d $dest | complete)
  if $ext.exit_code != 0 {
    rm -rf $dest
    quarantine $zip_path $failed_dir "extraction failed"
    return $skipped
  }

  let row = (describe-album $dest)
  if $row == null {
    rm -rf $dest
    quarantine $zip_path $failed_dir "no audio files inside"
    return $skipped
  }

  let target = $"($music_root)/($row.artist)/($row.album)"
  print $"→ ($row.artist) — ($row.album) \(($row.tracks) tracks\) -> ($target)"
  let xfer = (^rsync -a --mkpath $"($row.src)/" $"($target)/" | complete)
  rm -rf $dest
  if $xfer.exit_code != 0 {
    quarantine $zip_path $failed_dir $"rsync to ($target) failed: ($xfer.stderr | str trim | str substring 0..120)"
    return ($skipped | merge { artist: $row.artist, album: $row.album })
  }

  let lid = (if $lidarr == null {
    "skipped"
  } else {
    (lidarr-register $lidarr_url $lidarr.key $lidarr.root $lidarr.qid $lidarr.mid $row.artist).status
  })

  mv $zip_path (unique-dest $archive_dir $name)
  { zip: $name, artist: $row.artist, album: $row.album, imported: true, lidarr: $lid }
}

# Non-interactive drop-directory importer, run on the media host itself by a
# systemd path unit watching <watch_dir>/incoming for *.zip.
#
# Contract with PathExistsGlob: on a clean exit no zip may remain in incoming/
# — systemd re-checks the glob when the service deactivates and would re-fire
# immediately, forever. Zips still being written (mtime younger than
# --settle-seconds, e.g. an in-flight scp) are waited on *inside* this run;
# everything else is moved to archive/ (imported) or failed/ (quarantined).
# --max-wait-seconds bounds the waiting so a trickling upload can't wedge the
# unit; the next trigger simply picks it up.
def "main batch" [
  watch_dir: path                # root dir; incoming/, failed/, archive/ live beneath it
  --music-root: string = "/data/music"
  --lidarr-url: string = "http://127.0.0.1:8686"
  --lidarr-config: string = "/data/apps/lidarr/config.xml"  # local config.xml holding the API key
  --no-lidarr                    # skip Lidarr; just place files for Navidrome
  --settle-seconds: int = 60     # a zip is safe to touch once its mtime is at least this old
  --max-wait-seconds: int = 900  # stop waiting for unsettled zips after this long
] {
  let incoming = ($watch_dir | path join "incoming")
  let failed_dir = ($watch_dir | path join "failed")
  let archive_dir = ($watch_dir | path join "archive")
  # Extraction scratch on the same pool as the drop dir — multi-GB albums
  # would not fit a tmpfs /tmp. Dot-prefixed so the watch glob never sees it.
  let work = ($watch_dir | path join ".work")

  for d in [$incoming $failed_dir $archive_dir] { mkdir $d }
  if ($work | path exists) { rm -rf $work }
  mkdir $work

  let lidarr = (if $no_lidarr {
    null
  } else if not ($lidarr_config | path exists) {
    print -e $"⚠ ($lidarr_config) not found; importing without Lidarr registration."
    null
  } else {
    let key = (parse-api-key (open --raw $lidarr_config))
    if $key == null {
      print -e $"⚠ no ApiKey in ($lidarr_config); importing without Lidarr registration."
      null
    } else {
      dbg $"API key read from ($lidarr_config): length ($key | str length)"
      let ctx = (lidarr-context $lidarr_url $key)
      if $ctx == null {
        print -e $"⚠ Lidarr API at ($lidarr_url) unreachable; importing without Lidarr registration."
      }
      $ctx
    }
  })

  let started = (date now)
  mut results = []
  loop {
    let zips = (
      ls $incoming
      | where type == file
      | where {|f| $f.name | str lowercase | str ends-with ".zip" }
    )
    if ($zips | is-empty) { break }

    let cutoff = ((date now) - ($settle_seconds * 1sec))
    let settled = ($zips | where modified < $cutoff)

    for z in $settled {
      # An unexpected error must not leave the zip in incoming/ (see the
      # contract above), so even the catch-all quarantines.
      let row = (try {
        import-one $z.name $work $failed_dir $archive_dir $music_root $lidarr_url $lidarr
      } catch {|e|
        if ($z.name | path exists) {
          quarantine $z.name $failed_dir $"unexpected error: ($e.msg | str substring 0..120)"
        }
        { zip: ($z.name | path basename), artist: null, album: null, imported: false, lidarr: "skipped" }
      })
      $results = ($results | append $row)
    }

    if (($zips | length) == ($settled | length)) { continue }
    if ((date now) - $started) > ($max_wait_seconds * 1sec) {
      print -e $"⚠ giving up on (($zips | length) - ($settled | length)) still-changing zip\(s\); next trigger retries."
      break
    }
    sleep 15sec
  }

  rm -rf $work

  if ($results | is-empty) {
    print "Nothing to import."
    return
  }
  print ($results | select zip artist album imported lidarr)
  let failed_count = ($results | where imported == false | length)
  let ok_count = (($results | length) - $failed_count)
  let unmatched = ($results | where lidarr == "no-match" | length)
  if $unmatched > 0 {
    print $"($unmatched) artist\(s\) not in MusicBrainz — placed for Navidrome only."
  }
  print $"✓ ($ok_count) imported, ($failed_count) quarantined."
  if $failed_count > 0 { exit 1 }
}
