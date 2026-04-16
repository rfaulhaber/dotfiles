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

let input_changes = if ($"($run_dir)/input-changes.json" | path exists) {
    open $"($run_dir)/input-changes.json" | from json
} else {
    print $"WARN: ($run_dir)/input-changes.json missing — reporting no input changes."
    []
}

let build_results = (glob $"($run_dir)/build-report-*.json"
    | each {|f| open $f | from json }
    | sort-by host)

let report = {
    input_changes: $input_changes
    build_results: $build_results
}

$report | to json | save -f build-report.json

let successes = ($build_results | where status == "success" | length)
let failures = ($build_results | where status == "failed" | length)
print $"=== Merged report: ($successes) succeeded, ($failures) failed ==="
