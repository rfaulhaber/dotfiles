#!/usr/bin/env nu

# Refresh registry digests for every entry in each host's oci-images.json.
# Walks the JSON to find leaf records containing both "version" and "digest",
# looks up each entry's repository via `nix eval` against the host config,
# fetches the current registry digest with skopeo, and rewrites the JSON in
# place when digests have moved. After rewriting, evaluates each affected
# host's toplevel as a sanity check.
#
# Entries pinned to an explicit version get a second, read-only check: the
# registry's tag list is scanned for a newer release on the same tag line.
# A digest refresh can never move those pins — that is the point of pinning —
# so without this a pinned service drifts years behind in total silence.
#
# This script does NOT touch git — it leaves modified files in the working
# tree for the caller (a CI follow-up step, or a human running locally) to
# review and commit. See .github/scripts/commit-oci-digests.nu for the CI
# git side.
#
# Env:
#   DRY_RUN — when "true", print proposed changes without writing files
#             or evaluating. The version check still runs; it only reads.
#   OCI_REPORT_DIR — any writable dir; receives oci-changes.json,
#                    oci-fetch-failures.json, oci-validation-failures.json
#                    and oci-version-warnings.json. Defaults to the cwd.
#
# Outputs (via $env.GITHUB_OUTPUT):
#   changed        — "true" if any digest moved
#   date           — UTC date stamp for downstream branch/PR naming
#   fetch_failures — count of entries whose repo or tag would not resolve
#   newer_versions — count of pinned entries with a newer upstream version

let dry_run = ($env.DRY_RUN? | default "false") == "true"

# skopeo >= 1.23 refuses to parse the legacy v1 /etc/containers/registries.conf
# that the NixOS `virtualisation.containers` module still emits. Every repo in
# oci-images.json is fully qualified, so the search-registries list is never
# consulted — point skopeo at a minimal valid v2 file to sidestep the host's.
let registries_conf = $nu.temp-dir | path join "oci-digests-registries.conf"
'unqualified-search-registries = ["docker.io"]' | save -f $registries_conf
$env.CONTAINERS_REGISTRIES_CONF = $registries_conf

# Without REGISTRY_AUTH_FILE or XDG_RUNTIME_DIR, skopeo's credential lookup
# falls back to /run/containers/<uid>/auth.json, and on a root-podman host
# that directory is 0700 root — a non-root run (e.g. the DynamicUser CI
# runner) gets EACCES, which skopeo treats as fatal, unlike ENOENT. Point it
# at a nonexistent file: every repo here is public, anonymous is correct.
$env.REGISTRY_AUTH_FILE = $nu.temp-dir | path join "oci-digests-auth.json"

# Walk a parsed JSON tree and yield {path, version, digest} for every leaf
# record that has both "version" and "digest" keys. Path is a list of
# attribute names (e.g. ["immich" "postgres" "image"]).
def find-image-specs [tree, prefix: list<any> = []] {
    if (($tree | describe) | str starts-with "record") {
        let cols = $tree | columns
        if ("version" in $cols) and ("digest" in $cols) {
            [
                {path: $prefix, version: $tree.version, digest: $tree.digest}
            ]
        } else {
            $cols
            | each {|k| find-image-specs ($tree | get $k) ($prefix ++ [$k]) }
            | flatten
        }
    } else {
        []
    }
}

# Replace the leaf at `path` inside `tree` with `value`. Walks the tree
# rather than relying on cell-path syntax so attribute names with dashes
# (e.g. "forgejo-runner") work without quoting.
def deep-set [tree, path: list<any>, value] {
    if ($path | is-empty) {
        $value
    } else {
        let key = $path | first
        let rest = $path | skip 1
        $tree | upsert $key (deep-set ($tree | get $key) $rest $value)
    }
}

# Look up the module-defined repository for an image at services.<path>.
# Each path segment is double-quoted so dashes (forgejo-runner, calibre-web-auto)
# and reserved-looking names parse correctly in nix attribute expressions.
# Returns {ok, repository, error} rather than raising, so one unresolvable
# entry cannot abort the whole run — see the failure handling in main.
def query-repo [host: string, path: list<any>] {
    let segments = $path | each {|p| $'"($p)"' } | str join "."
    let attr = $".#nixosConfigurations.($host).config.modules.linux.oci.services.($segments)"
    let result = (^nix eval --json $attr | complete)
    if $result.exit_code != 0 {
        {ok: false, repository: null, error: $"nix eval failed for ($attr): ($result.stderr | str trim)"}
    } else {
        {ok: true, repository: ($result.stdout | from json | get repository), error: null}
    }
}

# The tag a leaf's digest must be fetched for is not always its bare
# `version`: GPU-variant images get a suffix appended at render time
# (mkGpuImage turns immich-ml's version "release" into tag
# "release-openvino" on an intel host). Rather than re-deriving per-module
# suffix rules, read the host's rendered container definitions — the exact
# references podman will pull — and take the tag from the reference whose
# repository and digest match the leaf.
def rendered-images [host: string] {
    let attr = $".#nixosConfigurations.($host).config.virtualisation.oci-containers.containers"
    let result = (^nix eval --json $attr --apply "builtins.mapAttrs (_: c: c.image)" | complete)
    if $result.exit_code != 0 {
        print -e $"WARN: could not read rendered containers for ($host); falling back to bare version tags: ($result.stderr | str trim)"
        []
    } else {
        $result.stdout | from json | values
    }
}

# Entries with no rendered match fall back to the bare version — that covers
# pinned services the host currently disables, at the cost of fetching the
# unsuffixed tag for a *disabled* GPU service. An enabled one always matches.
def resolve-tag [rendered: list<string>, repo: string, spec] {
    let hits = ($rendered | where {|r|
        ($r | str starts-with $"($repo):") and ($r | str ends-with $"@($spec.digest)")
    })
    if ($hits | is-empty) {
        $spec.version
    } else {
        $hits | first | str substring (($repo | str length) + 1).. | split row "@" | first
    }
}

# Resolve the current manifest digest of repo:tag against the registry.
# `--no-tags` skips the post-inspect /tags/list call we don't need.
# Returns {ok, digest, error} rather than raising: upstreams retire tags
# unilaterally (recyclarr dropped :latest mid-8.x), and a tag that no longer
# exists must not be able to freeze every other host's refresh.
def fetch-digest [repo: string, tag: string] {
    let url = $"docker://($repo):($tag)"
    let result = (^skopeo inspect --no-tags --format "{{.Digest}}" $url | complete)
    if $result.exit_code != 0 {
        {ok: false, digest: null, error: $"skopeo inspect failed for ($url): ($result.stderr | str trim)"}
    } else {
        {ok: true, digest: ($result.stdout | str trim), error: null}
    }
}

# --- Newer-version detection for pinned tags ---
#
# Container tags are not semver and cannot be parsed as such: the fleet pins
# "17.2-alpine", "v3.4.0", "14-vectorchord0.4.3-pgvectors0.2.0" and bare "8".
# Instead each tag is reduced to a *shape* — every digit run replaced by "#" —
# and only tags sharing a shape are compared, positionally, on the integers
# they contain. Identical shapes therefore always yield equal-length number
# lists, which is what makes the comparison total.
#
# Shape matching buys three properties that a looser comparison loses:
#   - variant lines stay apart: "17.2-alpine" (#.#-alpine) never compares
#     against "17.2-alpine3.16" (#.#-alpine#.#), which is a different image
#   - the pin's own precision is honoured: recyclarr's "8" (#) is measured
#     against "9", not "8.4.1" — tag 8 already floats onto 8.4.1, so the only
#     news worth reporting there is a new major
#   - floating tags exclude themselves, having no leading digit run at all
#
# The cost is under-reporting when upstream renames a tag component (immich's
# postgres went "-pgvectors" to "-pgvector", a shape change), which is the
# right way to be wrong: a missed warning is noise-free, a false one is not.

def tag-shape [tag: string] {
    $tag | str replace --all --regex '\d+' '#'
}

def tag-nums [tag: string] {
    $tag | parse --regex '(?<n>\d+)' | get n | each {|x| $x | into int }
}

# Only tags that *lead* with a version are pins. This rejects "latest",
# "release-openvino" and "nightly", but also "main-20260101", where the digits
# are a build stamp rather than a version to compare.
def pinned-tag? [tag: string] {
    (tag-shape $tag) =~ '^v?#'
}

# Callers must pass number lists of equal length — guaranteed by only ever
# comparing tags of the same shape.
def version-gt [a: list<int>, b: list<int>] {
    for i in 0..<($a | length) {
        let x = ($a | get $i)
        let y = ($b | get $i)
        if $x > $y {
            return true
        } else if $x < $y {
            return false
        }
    }
    false
}

# Returns {ok, tags, error}. Like fetch-digest, a failure is carried rather
# than raised: this whole check is advisory, and a registry that refuses a
# tag listing must not cost the run its digest refresh.
def list-tags [repo: string] {
    let result = (^skopeo list-tags $"docker://($repo)" | complete)
    if $result.exit_code != 0 {
        {ok: false, tags: [], error: $"skopeo list-tags failed for ($repo): ($result.stderr | str trim)"}
    } else {
        {ok: true, tags: ($result.stdout | from json | get Tags), error: null}
    }
}

# Greatest same-shape tag above `tag`, plus how many there are. Returns null
# when the pin is already current.
def newer-than [tags: list<string>, tag: string] {
    let shape = (tag-shape $tag)
    let current = (tag-nums $tag)
    let newer = ($tags | where {|t| (tag-shape $t) == $shape and (version-gt (tag-nums $t) $current) })
    if ($newer | is-empty) {
        null
    } else {
        let latest = ($newer | reduce --fold ($newer | first) {|t, acc|
            if (version-gt (tag-nums $t) (tag-nums $acc)) { $t } else { $acc }
        })
        {latest: $latest, count: ($newer | length)}
    }
}

# --- Main ---

def main [...hosts: string]: nothing -> nothing {
  let canonical_hosts = (
    ls nix/hosts/*/oci-images.json
        | each { |f|
            {host: ($f.name | path dirname | path basename), file: $f.name}
    })

    let hosts = if (not ($hosts | is-empty)) {
      $hosts
        | reduce --fold [] {|it, acc|
            let pred = { $in.host == $it }
            if ($it in ($canonical_hosts | get host)) {
              let host_info = $canonical_hosts | where $pred
              $acc | append $host_info
            }
        }
    } else {
      $canonical_hosts
    }

    # Each entry yields a "change", "unchanged" or "failure" record. Failures
    # are carried rather than raised: an entry can stop resolving for reasons
    # entirely outside this repo — an upstream retiring a tag is enough — and
    # aborting there would discard every other host's already-computed
    # digests, including hosts not yet inspected. Unchanged entries are kept
    # (rather than dropped as null) because a pinned tag whose digest never
    # moves is precisely what the newer-version check needs to see.
    let per_host = ($hosts | each { |h|
    print $"=== Inspecting ($h.host) ==="
    let manifest = (open $h.file)
    let specs = (find-image-specs $manifest)
    let rendered = (rendered-images $h.host)
    print $"  ($specs | length) image\(s\) declared"

    let host_results = ($specs | par-each { |s|
        let path_str = $s.path | str join "."
        let repo_result = (query-repo $h.host $s.path)
        if not $repo_result.ok {
            print $"  FAIL    ($path_str): ($repo_result.error)"
            {
                kind: "failure"
                host: $h.host
                path: $s.path
                repo: null
                version: $s.version
                error: $repo_result.error
            }
        } else {
            let repo = $repo_result.repository
            let tag = (resolve-tag $rendered $repo $s)
            let fetched = (fetch-digest $repo $tag)
            if not $fetched.ok {
                print $"  FAIL    ($path_str): ($fetched.error)"
                {
                    kind: "failure"
                    host: $h.host
                    path: $s.path
                    repo: $repo
                    version: $s.version
                    error: $fetched.error
                }
            } else if $fetched.digest == $s.digest {
                {
                    kind: "unchanged"
                    host: $h.host
                    path: $s.path
                    repo: $repo
                    version: $s.version
                    tag: $tag
                }
            } else {
                print $"  CHANGE  ($path_str): ($s.digest | str substring 0..19)… → ($fetched.digest | str substring 0..19)…"
                {
                    kind: "change"
                    host: $h.host
                    path: $s.path
                    repo: $repo
                    version: $s.version
                    tag: $tag
                    old_digest: $s.digest
                    new_digest: $fetched.digest
                }
            }
        }
    })

    let host_changes = ($host_results | where kind == "change")

    if (not ($host_changes | is-empty)) and (not $dry_run) {
        let new_manifest = ($host_changes | reduce -f $manifest { |c, m|
        deep-set $m ($c.path ++ ["digest"]) $c.new_digest
        })
        $new_manifest | to json --indent 2 | save -f $h.file
    }

    {attempted: ($specs | length), results: $host_results}
    })

    let all_results = ($per_host | get results | flatten)
    let attempted = ($per_host | reduce --fold 0 {|it, acc| $acc + $it.attempted})

    let all_changes = ($all_results | where kind == "change")
    let fetch_failures = ($all_results | where kind == "failure")

    print ""
    let output_file = $env.GITHUB_OUTPUT? | default "/dev/null"
    # Defaults to /tmp, not the CWD: the documented way to run this by hand is
    # from the repo root, and dropping report JSON there leaves untracked files
    # sitting beside the very diff you are meant to review. CI always sets this.
    let report_dir = $env.OCI_REPORT_DIR? | default "/tmp"
    mkdir $report_dir

    # Written before the no-changes early exit: an entry that never resolves
    # produces no change, so this is the only record that it was even tried.
    $fetch_failures | to json | save -f $"($report_dir)/oci-fetch-failures.json"

    if (not ($fetch_failures | is-empty)) {
        error $"($fetch_failures | length) image\(s\) failed to resolve — these are NOT refreshed:"
        for f in $fetch_failures {
            let path_str = ($f.path | str join ".")
            print $"     ($f.host) ($path_str): ($f.error)"
        }
        print ""
    }

    # A few entries failing is upstream churn and belongs in the report the PR
    # renders. Every entry failing at once cannot be — it means skopeo, the
    # registries.conf workaround, or registry egress is broken. Without this
    # that scenario yields no diff and reads as a quiet, healthy no-change run.
    if ($attempted > 0) and (($fetch_failures | length) == $attempted) {
        print -e $"All ($attempted) image\(s\) failed to resolve — this is a tooling or network failure, not upstream churn."
        exit 1
    }

    # Runs ahead of every early exit below, and regardless of DRY_RUN: a pinned
    # tag produces no digest movement by design, so the weeks where this has
    # something to say are exactly the weeks that otherwise print "No digest
    # updates." and stop.
    let pinned = ($all_results
        | where kind != "failure"
        | where {|r| pinned-tag? $r.tag })

    # One tag listing per repository, not per entry: postgres alone is pinned
    # at three different tags across the fleet, and podman-exporter at the same
    # tag on three hosts.
    let tag_lists = ($pinned
        | get repo
        | uniq
        | par-each {|repo| {repo: $repo, result: (list-tags $repo)} }
        | reduce --fold {} {|it, acc| $acc | upsert $it.repo $it.result })

    # A repo whose tags cannot be listed contributes no warnings, which is
    # indistinguishable from a pin that is current. Say so rather than let the
    # check rot into a permanent all-clear.
    for entry in ($tag_lists | transpose repo result | where {|e| not $e.result.ok }) {
        print -e $"WARN: version check skipped for ($entry.repo): ($entry.result.error)"
    }

    let version_warnings = ($pinned | each {|p|
        let listing = ($tag_lists | get $p.repo)
        if not $listing.ok {
            null
        } else {
            let newer = (newer-than $listing.tags $p.tag)
            if $newer == null {
                null
            } else {
                {
                    host: $p.host
                    path: $p.path
                    repo: $p.repo
                    tag: $p.tag
                    latest: $newer.latest
                    newer_count: $newer.count
                }
            }
        }
    } | compact)

    $version_warnings | to json | save -f $"($report_dir)/oci-version-warnings.json"

    if (not ($version_warnings | is-empty)) {
        warn $"($version_warnings | length) pinned image\(s\) have a newer upstream version — a digest refresh cannot move these:"
        for w in $version_warnings {
            let path_str = ($w.path | str join ".")
            print $"     ($w.host) ($path_str): ($w.repo):($w.tag) → ($w.latest) \(($w.newer_count) newer tag\(s\)\)"
        }
        print ""
    }

    # The job summary is written here rather than from the PR step because the
    # PR step only runs when a digest moved. A stale pin is most likely to show
    # up in a week with no diff at all, where this is the only surface it has.
    if (not ($version_warnings | is-empty)) {
        let summary_file = ($env.GITHUB_STEP_SUMMARY? | default "/dev/null")
        let rows = ($version_warnings | each {|w|
            let path_str = ($w.path | str join ".")
            $"| `($w.host)` | `($path_str)` | `($w.repo):($w.tag)` | `($w.latest)` |"
        } | str join "\n")
        ($"## Pinned images behind upstream\n\n($version_warnings | length) pinned image\(s\) have a newer version on the same tag line. "
            + "Digest refreshes cannot move a pinned tag — bump `version` in the host's `oci-images.json` and re-run this workflow.\n\n"
            + $"| Host | Module path | Pinned | Newest |\n| --- | --- | --- | --- |\n($rows)\n\n")
        | save --append $summary_file
    }

    if ($all_changes | is-empty) {
        print "No digest updates."
        ($"changed=false\ndate=\nfetch_failures=($fetch_failures | length)"
            + $"\nnewer_versions=($version_warnings | length)\n") | save --append $output_file
        exit 0
    }

    let affected_hosts = $all_changes | get host | uniq
    print $"Updated ($all_changes | length) image\(s\) across ($affected_hosts | length) host\(s\)."

    if $dry_run {
        print "DRY_RUN=true — not writing files or validating."
        ($"changed=false\ndate=\nfetch_failures=($fetch_failures | length)"
            + $"\nnewer_versions=($version_warnings | length)\n") | save --append $output_file
        exit 0
    }

    # Validate by evaluating each affected host's toplevel. Catches any
    # eval-time damage from the JSON rewrite (malformed digest, etc.) before
    # the change reaches a PR. A host can fail here for reasons that have
    # nothing to do with this digest bump (e.g. an unrelated pre-existing
    # eval break), so a failure is collected and surfaced in the PR rather
    # than aborting the run — one broken host must not discard every other
    # host's already-rewritten digests.
    print ""
    print "=== Validating affected hosts ==="
    let validation_failures = ($affected_hosts | each { |h|
        print $"  nix eval ($h)"
        let result = (
            ^nix eval --raw $".#nixosConfigurations.($h).config.system.build.toplevel.drvPath"
            | complete
        )
        if $result.exit_code != 0 {
            print $"    FAIL: ($result.stderr)"
            {host: $h, stderr: $result.stderr}
        } else {
            null
        }
    } | compact)

    let date_str = date now | date to-timezone UTC | format date "%Y%m%d"

    $all_changes | to json | save -f $"($report_dir)/oci-changes.json"
    $validation_failures | to json | save -f $"($report_dir)/oci-validation-failures.json"

    ($"changed=true\ndate=($date_str)\nfetch_failures=($fetch_failures | length)"
        + $"\nnewer_versions=($version_warnings | length)\n") | save --append $output_file
}

def warn [--error, message: string]: nothing -> nothing {
  message-with-color $error yellow $message
}

def error [--error (-e), message: string]: nothing -> nothing {
  message-with-color $error red $message
}

def message-with-color [error: bool, color: string, message: string]: nothing -> nothing {
  if $error {
    print -e $"(ansi $color)($message)(ansi reset)"
  } else {
    print $"(ansi $color)($message)(ansi reset)"
  }
}
