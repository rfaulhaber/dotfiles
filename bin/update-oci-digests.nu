#!/usr/bin/env nu

# Refresh registry digests for every entry in each host's oci-images.json.
# Walks the JSON to find leaf records containing both "version" and "digest",
# looks up each entry's repository via `nix eval` against the host config,
# fetches the current registry digest with skopeo, and rewrites the JSON in
# place when digests have moved. After rewriting, evaluates each affected
# host's toplevel as a sanity check.
#
# This script does NOT touch git — it leaves modified files in the working
# tree for the caller (a CI follow-up step, or a human running locally) to
# review and commit. See .github/scripts/commit-oci-digests.nu for the CI
# git side.
#
# Env:
#   DRY_RUN — when "true", print proposed changes without writing files
#             or evaluating.
#   OCI_REPORT_DIR — any writable dir; receives oci-changes.json,
#                    oci-fetch-failures.json and
#                    oci-validation-failures.json. Defaults to the cwd.
#
# Outputs (via $env.GITHUB_OUTPUT):
#   changed        — "true" if any digest moved
#   date           — UTC date stamp for downstream branch/PR naming
#   fetch_failures — count of entries whose repo or tag would not resolve

let dry_run = ($env.DRY_RUN? | default "false") == "true"

# skopeo >= 1.23 refuses to parse the legacy v1 /etc/containers/registries.conf
# that the NixOS `virtualisation.containers` module still emits. Every repo in
# oci-images.json is fully qualified, so the search-registries list is never
# consulted — point skopeo at a minimal valid v2 file to sidestep the host's.
let registries_conf = $nu.temp-dir | path join "oci-digests-registries.conf"
'unqualified-search-registries = ["docker.io"]' | save -f $registries_conf
$env.CONTAINERS_REGISTRIES_CONF = $registries_conf

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

    # Each entry yields either a "change" record, a "failure" record, or null
    # (digest unmoved). Failures are carried rather than raised: an entry can
    # stop resolving for reasons entirely outside this repo — an upstream
    # retiring a tag is enough — and aborting there would discard every other
    # host's already-computed digests, including hosts not yet inspected.
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
                null
            } else {
                print $"  CHANGE  ($path_str): ($s.digest | str substring 0..19)… → ($fetched.digest | str substring 0..19)…"
                {
                    kind: "change"
                    host: $h.host
                    path: $s.path
                    repo: $repo
                    version: $s.version
                    old_digest: $s.digest
                    new_digest: $fetched.digest
                }
            }
        }
    } | compact)

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
        print $"!! ($fetch_failures | length) image\(s\) failed to resolve — these are NOT refreshed:"
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

    if ($all_changes | is-empty) {
        print "No digest updates."
        $"changed=false\ndate=\nfetch_failures=($fetch_failures | length)\n" | save --append $output_file
        exit 0
    }

    let affected_hosts = $all_changes | get host | uniq
    print $"Updated ($all_changes | length) image\(s\) across ($affected_hosts | length) host\(s\)."

    if $dry_run {
        print "DRY_RUN=true — not writing files or validating."
        $"changed=false\ndate=\nfetch_failures=($fetch_failures | length)\n" | save --append $output_file
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

    $"changed=true\ndate=($date_str)\nfetch_failures=($fetch_failures | length)\n" | save --append $output_file
}
