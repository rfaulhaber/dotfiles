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
# review and commit. See commit-oci-digests.nu for the CI git side.
#
# Env:
#   DRY_RUN — when "true", print proposed changes without writing files
#             or evaluating.
#   CI_RUN_DIR — bind-mounted /ci-state subdir (in CI) or any writable dir
#                (locally); receives oci-changes.json. Falls back to /tmp.
#
# Outputs (via $env.GITHUB_OUTPUT):
#   changed — "true" if any digest moved
#   date    — UTC date stamp for downstream branch/PR naming

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
def query-repo [host: string, path: list<any>] {
    let segments = $path | each {|p| $'"($p)"' } | str join "."
    let attr = $".#nixosConfigurations.($host).config.modules.linux.oci.services.($segments)"
    let result = (^nix eval --json $attr | complete)
    if $result.exit_code != 0 {
        error make {msg: $"nix eval failed for ($attr):\n($result.stderr)"}
    }
    $result.stdout | from json | get repository
}

# Resolve the current manifest digest of repo:tag against the registry.
# `--no-tags` skips the post-inspect /tags/list call we don't need.
def fetch-digest [repo: string, tag: string] {
    let url = $"docker://($repo):($tag)"
    let result = (^skopeo inspect --no-tags --format "{{.Digest}}" $url | complete)
    if $result.exit_code != 0 {
        error make {msg: $"skopeo inspect failed for ($url):\n($result.stderr)"}
    }
    $result.stdout | str trim
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

    let all_changes = ($hosts | each { |h|
    print $"=== Inspecting ($h.host) ==="
    let manifest = (open $h.file)
    let specs = (find-image-specs $manifest)
    print $"  ($specs | length) image\(s\) declared"

    let host_changes = ($specs | each { |s|
        let repo = (query-repo $h.host $s.path)
        let new_digest = (fetch-digest $repo $s.version)
        if $new_digest == $s.digest {
        null
        } else {
        let path_str = $s.path | str join "."
        print $"  CHANGE  ($path_str): ($s.digest | str substring 0..19)… → ($new_digest | str substring 0..19)…"
        {
            host: $h.host
            path: $s.path
            repo: $repo
            version: $s.version
            old_digest: $s.digest
            new_digest: $new_digest
        }
        }
    } | compact)

    if (not ($host_changes | is-empty)) and (not $dry_run) {
        let new_manifest = ($host_changes | reduce -f $manifest { |c, m|
        deep-set $m ($c.path ++ ["digest"]) $c.new_digest
        })
        $new_manifest | to json --indent 2 | save -f $h.file
    }

    $host_changes
    } | flatten)

    print ""
    let output_file = $env.GITHUB_OUTPUT? | default "/dev/null"

    if ($all_changes | is-empty) {
        print "No digest updates."
        $"changed=false\ndate=\n" | save --append $output_file
        exit 0
    }

    let affected_hosts = $all_changes | get host | uniq
    print $"Updated ($all_changes | length) image\(s\) across ($affected_hosts | length) host\(s\)."

    if $dry_run {
        print "DRY_RUN=true — not writing files or validating."
        $"changed=false\ndate=\n" | save --append $output_file
        exit 0
    }

    # Validate by evaluating each affected host's toplevel. Catches any
    # eval-time damage from the JSON rewrite (malformed digest, etc.) before
    # the change reaches a PR.
    print ""
    print "=== Validating affected hosts ==="
    for h in $affected_hosts {
        print $"  nix eval ($h)"
        let result = (
            ^nix eval --raw $".#nixosConfigurations.($h).config.system.build.toplevel.drvPath"
            | complete
        )
        if $result.exit_code != 0 {
            print $"    FAIL: ($result.stderr)"
            error make {msg: $"validation failed for ($h)"}
        }
    }

    let date_str = date now | date to-timezone UTC | format date "%Y%m%d"

    let report_dir = $env.CI_RUN_DIR? | default "/tmp"
    mkdir $report_dir
    $all_changes | to json | save -f $"($report_dir)/oci-changes.json"

    $"changed=true\ndate=($date_str)\n" | save --append $output_file
}
