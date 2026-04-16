#!/usr/bin/env nu

# Update all flake inputs, build all Linux NixOS hosts, and write a build report.
# Builds run against the container's local /nix/store (fast; uses cache.nixos.org).
# After each successful build, the resulting closure is copied to the host's
# nix daemon (socket mounted at /nix/var/nix/daemon-socket/socket) so harmonia
# can serve it as a binary cache.
#
# NOTE: We intentionally do NOT set NIX_REMOTE=daemon. Mixing a container-local
# /nix/store with the host daemon's /nix/store breaks flake input fetching: the
# client-side evaluator reads store paths via open(), but with NIX_REMOTE=daemon
# the fetched paths only exist on the host's filesystem — not inside the
# container — and evaluation fails with "opening file '...-source': No such
# file or directory".

let hosts = {
    x86_64: [hyperion atlas janus vulcan]
    aarch64: [pallas hecate]
}

let all_hosts = ($hosts.x86_64 | append $hosts.aarch64)

# --- Capture before state ---
print "=== Capturing pre-update flake metadata ==="
let before = (nix flake metadata --json | from json)

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
    let result = do {
        nix build $".#nixosConfigurations.($host).config.system.build.toplevel"
            --no-link
            --print-out-paths
            --print-build-logs
    } | complete
    let elapsed = (date now) - $start | format duration min

    if $result.exit_code == 0 {
        let out_paths = ($result.stdout | lines | each {|l| $l | str trim } | where {|l| $l != "" })
        print $"  ✓ ($host) built successfully \(($elapsed)\)"
        { host: $host, status: "success", elapsed: $elapsed, error: "", paths: $out_paths }
    } else {
        let err_tail = ($result.stderr | lines | last 20 | str join "\n")
        print $"  ✗ ($host) failed \(($elapsed)\)"
        print $"    ($err_tail)"
        { host: $host, status: "failed", elapsed: $elapsed, error: $err_tail, paths: [] }
    }
})

# --- Copy successful closures to the host nix daemon ---
# The daemon socket is bind-mounted into the container, so "--to daemon"
# targets the host's store. --no-check-sigs is required because we build
# unsigned inside the container; root-over-the-socket is a trusted-user.
print "=== Copying built closures to host nix daemon ==="
for r in ($build_results | where status == "success") {
    if ($r.paths | is-empty) { continue }
    print $"--- Copying ($r.host) ---"
    let copy_result = do {
        nix copy --to daemon --no-check-sigs ...$r.paths
    } | complete
    if $copy_result.exit_code == 0 {
        print $"  ✓ ($r.host) copied to host store"
    } else {
        let err_tail = ($copy_result.stderr | lines | last 10 | str join "\n")
        print $"  ✗ ($r.host) copy failed"
        print $"    ($err_tail)"
    }
}

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
