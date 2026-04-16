#!/usr/bin/env nu

# Merge per-host build-report-<host>.json files (downloaded as artifacts into
# build-reports/) with input-changes.json (downloaded to the working dir) into
# a single build-report.json for create-pr.nu to consume.
#
# Also refreshes .ci/last-builds.json with the toplevel store paths of every
# successful build so the next flake-update run can seed its containers from
# the daemon. Failed hosts keep their previous entries — a one-off failure
# shouldn't lose the warm cache.
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

# --- Refresh .ci/last-builds.json for the next run's seed step ---
let state_file = ".ci/last-builds.json"
let existing = if ($state_file | path exists) {
    open $state_file
} else {
    {}
}

let refreshed = ($build_results
    | where status == "success"
    | reduce --fold $existing {|b, acc|
        $acc | upsert $b.host $b.paths
    })

$refreshed | to json | save -f $state_file

let successes = ($build_results | where status == "success" | length)
let failures = ($build_results | where status == "failed" | length)
print $"=== Merged report: ($successes) succeeded, ($failures) failed ==="
print $"=== Refreshed ($state_file) for ($successes) host\(s\) ==="
