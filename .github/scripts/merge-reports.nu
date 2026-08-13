#!/usr/bin/env nu

# Merge per-host build-report-<host>.json files with input-changes.json into a
# single build-report.json for create-pr.nu to consume. Both arrive in the
# workspace root as artifacts downloaded from the prepare and build jobs.

# `open` on a *.json file auto-parses into structured data, so piping the
# result through `from json` errors ("only string input data is supported").
# Let `open` do the parsing directly.
let input_changes = if ("input-changes.json" | path exists) {
    open input-changes.json
} else {
    print "WARN: input-changes.json missing — reporting no input changes."
    []
}

let build_results = (glob "build-report-*.json"
    | each {|f| open $f }
    | sort-by host)

# A matrix leg that dies before writing its report (OOM, runner eviction, a
# job timeout) would otherwise just vanish from this summary instead of
# failing it. .github/hosts.json is the same file the workflow reads to build
# the matrix in the first place, so this check can't drift out of sync with
# which hosts actually ran.
#
# The file is tracked in-repo, so a missing checkout means something is
# wrong with the environment (bad clone, wrong cwd) — treat that as a hard
# failure rather than silently skipping the very check it would defeat.
if not (".github/hosts.json" | path exists) {
    print --stderr "ERROR: .github/hosts.json missing — cannot verify report completeness."
    exit 1
}
let expected_hosts = (open .github/hosts.json | get host)
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
let warnings = ($build_results | each {|r| $r.warnings? | default [] } | flatten | uniq | length)
print $"=== Merged report: ($successes) succeeded, ($failures) failed, ($warnings) unique eval warnings ==="
