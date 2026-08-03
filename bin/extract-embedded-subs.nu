# Extract embedded text subtitle tracks into sidecar files beside the media.
#
# Jellyfin hands an external subtitle file straight to the client, but an
# embedded track goes through SubtitleEncoder.GetReadableFile, which only
# short-circuits when the stream is IsExternal. Everything else demuxes the
# whole container to collect a few KB of cues: Matroska interleaves subtitle
# blocks with video and audio in presentation order, so there is no index that
# reaches them without a linear read. On a large remux served over NFS that read
# saturates the link and stalls playback start by a minute or more. Writing the
# sidecar once, on the host that owns the bytes, removes it permanently.
#
# Bitmap subtitle codecs (PGS, VobSub, DVB) are skipped — converting those to
# text needs OCR, which is out of scope.
#
# The run is two phases: plan every extraction by probing headers, then execute
# the plan in parallel. Keeping them separate is what makes --limit and
# --dry-run deterministic, and it lets the plan be deduplicated before any
# bytes are written (see the uniq-by in `main`).

# Subtitle codecs that can become a text sidecar, and how to get there.
# `copy` keeps the original bytes; ASS/SSA stay ASS so styling survives.
def codec-target [codec: string] {
  match ($codec | str lowercase) {
    "subrip" => ({ext: "srt", encoder: "copy"})
    "ass" => ({ext: "ass", encoder: "copy"})
    "ssa" => ({ext: "ass", encoder: "copy"})
    "mov_text" => ({ext: "srt", encoder: "srt"})
    "webvtt" => ({ext: "srt", encoder: "srt"})
    "text" => ({ext: "srt", encoder: "srt"})
    _ => null
  }
}

# Extensions Jellyfin recognizes as external subtitles (NamingOptions.SubtitleFileExtensions).
def subtitle-extensions [] {
  [".ass" ".mks" ".sami" ".smi" ".srt" ".ssa" ".sub" ".sup" ".vtt"]
}

# ISO 639-2 -> 639-1 for languages that actually turn up in media, so an
# existing "Movie.en.srt" is recognized as covering an embedded "eng" track.
# A miss here is harmless: worst case is one redundant sidecar, written once,
# after which the exact-name check keeps the run idempotent.
def lang-forms [code: string] {
  let map = {
    eng: "en", spa: "es", fra: "fr", fre: "fr", deu: "de", ger: "de"
    ita: "it", jpn: "ja", por: "pt", rus: "ru", zho: "zh", chi: "zh"
    kor: "ko", nld: "nl", dut: "nl", swe: "sv", dan: "da", nor: "no"
    fin: "fi", pol: "pl", tur: "tr", ara: "ar", heb: "he", hun: "hu"
    ces: "cs", cze: "cs", ell: "el", gre: "el", tha: "th", vie: "vi"
    ind: "id", ukr: "uk", ron: "ro", rum: "ro"
  }
  let lower = ($code | str lowercase)
  let short = if $lower in ($map | columns) {$map | get $lower} else {null}

  [$lower] | append ($short | default []) | uniq
}

def field [rec: any, key: string, fallback: any] {
  if ($rec | describe | str starts-with "record") and $key in ($rec | columns) {
    $rec | get $key
  } else {
    $fallback
  }
}

# Subtitle files already sitting next to $video, as bare filenames.
def existing-sidecars [video: string] {
  let dir = ($video | path dirname)
  let stem = ($video | path parse | get stem)
  let exts = (subtitle-extensions)

  ls --short-names $dir
  | where type == file
  | get name
  | where {|n| $n | str starts-with $"($stem)."}
  | where {|n| $exts | any {|e| $n | str lowercase | str ends-with $e}}
}

# Does an existing sidecar already serve this language? A sidecar carrying no
# language token at all (plain "Movie.srt") is treated as covering everything —
# that is how Jellyfin will present it, since MediaStreamSelector sorts
# IsExternal ahead of every other key when picking the default track.
def language-covered [names: list<string>, stem: string, lang: string] {
  let wanted = (lang-forms $lang)

  $names | any {|n|
    let extra = ($n | str substring (($stem | str length) + 1)..)
    let tokens = ($extra | split row "." | drop 1 | where {|t| $t != ""})

    ($tokens | is-empty) or ($tokens | any {|t| ($t | str lowercase) in $wanted})
  }
}

def sidecar-path [
  video: string
  title: string
  lang: string
  forced: bool
  hearing_impaired: bool
  ext: string
] {
  let parsed = ($video | path parse)
  let clean_title = ($title | str trim)

  mut parts = [$parsed.stem]
  if ($clean_title | is-not-empty) {
    $parts = ($parts | append $clean_title)
  }
  $parts = ($parts | append $lang)
  if $forced {
    $parts = ($parts | append "forced")
  }
  if $hearing_impaired {
    $parts = ($parts | append "sdh")
  }

  [$parsed.parent (($parts | str join ".") + "." + $ext)] | path join
}

def probe-subtitle-streams [video: string] {
  let probe = (
    ^ffprobe -v quiet -print_format json -show_streams -select_streams s $video
    | complete
  )

  if $probe.exit_code != 0 {
    print --stderr $"  ffprobe failed \(exit ($probe.exit_code)\): ($video)"
    return []
  }

  let parsed = try {$probe.stdout | from json} catch {null}
  if $parsed == null {
    print --stderr $"  unparseable ffprobe output: ($video)"
    return []
  }

  field $parsed "streams" []
}

def find-videos [root: string] {
  if not ($root | path exists) {
    print --stderr $"Skipping missing root: ($root)"
    return []
  }

  ^find $root -type f -regextype posix-extended -iregex '.*\.(mkv|mp4|m4v|avi|ts|m2ts|webm)$'
  | lines
  | where {|l| ($l | str trim) != ""}
}

# Every extraction this video needs, as job records. Reads headers only, so it
# is cheap enough to run across the whole library before committing to work.
def plan-video [video: string, clean_title: string, force: bool] {
  let stem = ($video | path parse | get stem)
  let sidecars = (existing-sidecars $video)

  probe-subtitle-streams $video
  | each {|stream|
    let spec = (codec-target (field $stream "codec_name" ""))
    let index = (field $stream "index" null)

    if $spec == null or $index == null {
      null
    } else {
      let tags = (field $stream "tags" {})
      let lang = (field $tags "language" "und")
      let disposition = (field $stream "disposition" {})
      let forced = ((field $disposition "forced" 0) == 1)
      let hearing_impaired = ((field $disposition "hearing_impaired" 0) == 1)
      let target = (
        sidecar-path $video $clean_title $lang $forced $hearing_impaired $spec.ext
      )

      if ($target | path exists) {
        null
      } else if (not $force) and (language-covered $sidecars $stem $lang) {
        null
      } else {
        {
          video: $video
          index: $index
          language: $lang
          target: $target
          encoder: $spec.encoder
          ext: $spec.ext
        }
      }
    }
  }
}

def run-job [job: record, seq: int, total: int] {
  # The temp name carries our PID because the nightly timer and a manual
  # backfill can independently plan the same job; a shared ".part" would let
  # two ffmpeg processes interleave writes into one file and rename the result
  # into place. Distinct names make the overlap merely wasteful, not corrupting
  # — both produce identical bytes and `mv` is atomic within the dataset.
  # The trailing suffix also defeats ffmpeg's extension sniffing, so the muxer
  # is named explicitly with -f.
  let part = $"($job.target).part-($nu.pid)"
  let name = ($job.target | path basename)

  let run = (
    ^ffmpeg -nostdin -y -v error
      -i $job.video
      -map $"0:($job.index)" -an -vn -dn
      -c:s $job.encoder
      -f $job.ext
      $part
    | complete
  )

  let base = {
    video: $job.video
    stream: $job.index
    language: $job.language
    sidecar: $name
  }

  if $run.exit_code == 0 {
    mv $part $job.target
    print --stderr $"  [($seq)/($total)] ($job.language) -> ($name)"
    $base | insert status "extracted"
  } else {
    if ($part | path exists) {
      rm --force $part
    }
    print --stderr $"  [($seq)/($total)] FAILED ($job.language) on ($job.video): ($run.stderr | str trim)"
    $base | insert status "failed"
  }
}

def main [
  ...roots: string # media roots to sweep (default: /data/movies /data/tv)
  --title: string = "Fast Start" # token baked into the sidecar name; Jellyfin renders Title FIRST, so it survives label truncation on TV clients where the trailing "External" tag gets cut off
  --jobs (-j): int = 4 # concurrent ffmpeg extractions
  --limit: int = 0 # stop after this many extractions; 0 means unlimited
  --force # re-extract even when a sidecar already covers the language
  --dry-run
] {
  for tool in ["ffprobe" "ffmpeg" "find"] {
    if (which $tool | is-empty) {
      print --stderr $"Missing ($tool) on PATH. Exiting."
      exit 1
    }
  }

  if $jobs < 1 {
    error make {msg: $"--jobs must be at least 1, got ($jobs)"}
  }

  # Jellyfin's ExternalPathParser matches default/forced flags with Contains(),
  # not equality, so a title holding any of these substrings silently flips the
  # flag on every sidecar we write. Hearing-impaired flags match with Equals().
  let clean_title = ($title | str trim)
  let lower_title = ($clean_title | str lowercase)
  for bad in ["default" "forced" "foreign"] {
    if ($lower_title | str contains $bad) {
      error make {msg: $"--title cannot contain '($bad)': Jellyfin substring-matches it as a subtitle flag"}
    }
  }
  if $lower_title in ["cc" "hi" "sdh"] {
    error make {msg: $"--title cannot be '($clean_title)': Jellyfin reads it as a hearing-impaired flag"}
  }

  let roots = if ($roots | is-empty) {["/data/movies" "/data/tv"]} else {$roots}
  let videos = ($roots | each {|r| find-videos $r} | flatten)

  print --stderr $"Scanning ($videos | length) video files under ($roots | str join ', ')"
  if $dry_run {
    print --stderr "[DRY RUN] no files will be written"
  }

  # Planning only reads container headers, so it is seek-bound and overlaps far
  # better than the whole-file reads in the execute phase — hence the wider
  # thread count here. Extraction stays at --jobs because those reads are
  # bandwidth-bound and oversubscribing them only causes seek thrash.
  let planned = (
    $videos
    | par-each --threads ($jobs * 4) --keep-order {|v| plan-video $v $clean_title $force}
    | flatten
  )

  # Two streams can resolve to one filename — e.g. a file carrying two English
  # SubRip tracks, neither flagged. Sequentially the second was caught by the
  # path-exists check once the first was written; planned up front, nothing is
  # on disk yet, so both would survive and race on the same output.
  let deduped = if ($planned | is-empty) {[]} else {$planned | uniq-by target}
  let jobs_list = if $limit > 0 {$deduped | take $limit} else {$deduped}
  let total = ($jobs_list | length)

  if $limit > 0 and ($deduped | length) > $limit {
    print --stderr $"Planned ($deduped | length) extraction\(s\); --limit ($limit) caps this run."
  } else {
    print --stderr $"Planned ($total) extraction\(s\)."
  }

  if $dry_run {
    return (
      $jobs_list | each {|job| {
        video: $job.video
        stream: $job.index
        language: $job.language
        sidecar: ($job.target | path basename)
        status: "would-extract"
      }}
    )
  }

  let results = (
    $jobs_list
    | enumerate
    | par-each --threads $jobs {|it| run-job $it.item ($it.index + 1) $total}
  )

  let failed = ($results | where status == "failed" | length)
  print --stderr $"Done. ($results | where status == 'extracted' | length) sidecar\(s\) written, ($failed) failed, from ($videos | length) files scanned."

  $results
}
