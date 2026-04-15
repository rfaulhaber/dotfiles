#!/usr/bin/env nu

# Update all flake inputs, build all Linux NixOS hosts, and write a build report.
# Build outputs go directly into the host nix store via the daemon socket,
# where harmonia serves them as a binary cache.

let hosts = {
    x86_64: [hyperion atlas janus vulcan]
    aarch64: [pallas hecate]
}

let all_hosts = ($hosts.x86_64 | append $hosts.aarch64)

# --- Capture before state ---
print "=== Capturing pre-update flake metadata ==="
let before = (nix flake metadata --json | from json)

# Use the host's nix daemon for all subsequent operations.
# Flake update fetches inputs into the host store, and builds land there
# directly — no signing or copying needed since harmonia serves from it.
$env.NIX_REMOTE = "daemon"

# --- Update all inputs ---
print "=== Updating all flake inputs ==="
nix flake update

# --- Capture after state ---
let after = (nix flake metadata --json | from json)

# --- Compute input diffs ---
print "=== Computing input changes ==="
let root_inputs = $before.locks.nodes.root.inputs | columns
let before_nodes = $before.locks.nodes
let after_nodes = $after.locks.nodes

let input_changes = ($root_inputs | each {|input_name|
    let node_key = ($before_nodes.root.inputs | get $input_name)
    # node_key might be a string or a list (for indirect inputs); skip lists
    if ($node_key | describe | str starts-with "list") {
        null
    } else {
        let before_node = ($before_nodes | get $node_key)
        let after_node = ($after_nodes | get $node_key)
        let before_rev = ($before_node.locked.rev? | default "n/a")
        let after_rev = ($after_node.locked.rev? | default "n/a")
        if $before_rev == $after_rev {
            null
        } else {
            let before_date = if $before_rev != "n/a" {
                $before_node.locked.lastModified | into string | into datetime | format date "%Y-%m-%d"
            } else { "n/a" }
            let after_date = if $after_rev != "n/a" {
                $after_node.locked.lastModified | into string | into datetime | format date "%Y-%m-%d"
            } else { "n/a" }
            {
                input: $input_name
                before_rev: $"($before_rev | str substring 0..12)"
                before_date: $before_date
                after_rev: $"($after_rev | str substring 0..12)"
                after_date: $after_date
            }
        }
    }
} | compact)

print "Updated inputs:"
if ($input_changes | is-empty) {
    print "  (none changed)"
} else {
    $input_changes | table | print
}

# --- Build all hosts ---
print "=== Building NixOS configurations ==="

let build_results = ($all_hosts | each {|host|
    print $"--- Building ($host) ---"
    let start = date now
    let result = do { nix build $".#nixosConfigurations.($host).config.system.build.toplevel" --print-build-logs } | complete
    let elapsed = (date now) - $start | format duration min

    if $result.exit_code == 0 {
        print $"  ✓ ($host) built successfully \(($elapsed)\)"
        { host: $host, status: "success", elapsed: $elapsed, error: "" }
    } else {
        let err_tail = ($result.stderr | lines | last 20 | str join "\n")
        print $"  ✗ ($host) failed \(($elapsed)\)"
        print $"    ($err_tail)"
        { host: $host, status: "failed", elapsed: $elapsed, error: $err_tail }
    }
})

# --- Write report ---
let report = {
    input_changes: $input_changes
    build_results: $build_results
}

$report | to json | save -f build-report.json

let successes = ($build_results | where status == "success" | length)
let failures = ($build_results | where status == "failed" | length)
print $"=== Build complete: ($successes) succeeded, ($failures) failed ==="

# Fail the step if ALL builds failed
if $successes == 0 and ($all_hosts | length) > 0 {
    print "All builds failed, aborting."
    exit 1
}
