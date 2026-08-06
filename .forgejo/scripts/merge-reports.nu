#!/usr/bin/env nu

# Merge per-host build-report-<host>.json files with input-changes.json into a
# single build-report.json for create-pr.nu to consume. All per-run files are
# written by earlier jobs to $CI_RUN_DIR (a subdirectory of /ci-state that is
# bind-mounted into every job container on this runner), so no artifact
# uploads/downloads are needed to pass data between jobs.
#
# Seed state for future runs is persisted per-host into /ci-state/seed by
# each matrix leg itself, so this step does not touch any warm-cache state.

let run_dir = $env.CI_RUN_DIR

# `open` on a *.json file auto-parses into structured data, so piping the
# result through `from json` errors ("only string input data is supported").
# Let `open` do the parsing directly.
let input_changes = if ($"($run_dir)/input-changes.json" | path exists) {
    open $"($run_dir)/input-changes.json"
} else {
    print $"WARN: ($run_dir)/input-changes.json missing — reporting no input changes."
    []
}

let build_results = (glob $"($run_dir)/build-report-*.json"
    | each {|f| open $f }
    | sort-by host)

# A matrix leg that dies before writing its report (OOM, runner eviction, a
# crash outside build-one-host.nu's own error handling) would otherwise just
# vanish from this summary instead of failing it. .forgejo/hosts.json is the
# same file the workflow reads to build the matrix in the first place, so
# this check can't drift out of sync with which hosts actually ran.
#
# The file is tracked in-repo, so a missing checkout means something is
# wrong with the environment (bad clone, wrong cwd) — treat that as a hard
# failure rather than silently skipping the very check it would defeat.
if not (".forgejo/hosts.json" | path exists) {
    print --stderr "ERROR: .forgejo/hosts.json missing — cannot verify report completeness."
    exit 1
}
let expected_hosts = (open .forgejo/hosts.json | get host)
let actual_hosts = ($build_results | get host)
let missing_hosts = ($expected_hosts | where {|h| $h not-in $actual_hosts})

if not ($missing_hosts | is-empty) {
    print --stderr $"ERROR: no build report for ($missing_hosts | str join ', ') — matrix leg likely died before writing one."
    exit 1
}

let report = {
    input_changes: $input_changes
    build_results: $build_results
}

$report | to json | save -f build-report.json

let successes = ($build_results | where status == "success" | length)
let failures = ($build_results | where status == "failed" | length)
print $"=== Merged report: ($successes) succeeded, ($failures) failed ==="
