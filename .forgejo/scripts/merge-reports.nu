#!/usr/bin/env nu

# Merge per-host build-report-<host>.json files (downloaded as artifacts into
# build-reports/) with input-changes.json (downloaded to the working dir) into
# a single build-report.json for create-pr.nu to consume.
#
# Seed state for future runs is persisted per-host into /ci-state by each
# matrix leg itself, so this step does not touch any warm-cache state.
#
# forgejo/download-artifact@v4 with a pattern places each artifact in its own
# subdirectory, so the layout is:
#   build-reports/build-report-<host>/build-report-<host>.json
#   input-changes.json

let input_changes = if ("input-changes.json" | path exists) {
    open input-changes.json | from json
} else {
    print "WARN: input-changes.json missing — reporting no input changes."
    []
}

let build_results = (glob "build-reports/**/build-report-*.json"
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
