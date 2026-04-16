#!/usr/bin/env nu

# Build a single NixOS configuration, copy its closure to the host nix daemon,
# and write build-report-<host>.json with the result. Intended to run in a
# strategy.matrix leg — one invocation per host.
#
# Before building, seeds the container's local /nix/store with the previous
# successful build's closure for this host (recorded in .ci/last-builds.json).
# Most derivations are shared between two consecutive NixOS toplevels, so the
# seed turns those into cache hits instead of re-fetching from cache.nixos.org.
#
# NOTE: We intentionally do NOT set NIX_REMOTE=daemon. Mixing a container-local
# /nix/store with the host daemon's /nix/store breaks flake input fetching: the
# client-side evaluator reads store paths via open(), but with NIX_REMOTE=daemon
# the fetched paths only exist on the host's filesystem — not inside the
# container — and evaluation fails with "opening file '...-source': No such
# file or directory".

def main [host: string] {
    seed_local_store $host

    print $"=== Building ($host) ==="
    let start = date now
    let result = ^nix build $".#nixosConfigurations.($host).config.system.build.toplevel" --no-link --print-out-paths --print-build-logs | complete

    let elapsed = (date now) - $start | format duration min

    let report = if $result.exit_code == 0 {
        let out_paths = ($result.stdout | lines | each {|l| $l | str trim } | where {|l| $l != "" })
        print $"  ✓ ($host) built successfully \(($elapsed)\)"

        # Copy the built closure to the host daemon so other machines can
        # pull it via harmonia. --no-check-sigs is required because we build
        # unsigned inside the container; the daemon socket's root user is a
        # trusted-user on vulcan.
        print $"=== Copying ($host) closure to host nix daemon ==="
        let copy_result = ^nix copy --to daemon --no-check-sigs ...$out_paths | complete
        if $copy_result.exit_code == 0 {
            print $"  ✓ ($host) copied to host store"
        } else {
            let err_tail = ($copy_result.stderr | lines | last 10 | str join "\n")
            print $"  ✗ ($host) copy failed"
            print $"    ($err_tail)"
        }

        { host: $host, status: "success", elapsed: $elapsed, error: "", paths: $out_paths }
    } else {
        let err_tail = ($result.stderr | lines | last 20 | str join "\n")
        print $"  ✗ ($host) failed \(($elapsed)\)"
        print $"    ($err_tail)"
        { host: $host, status: "failed", elapsed: $elapsed, error: $err_tail, paths: [] }
    }

    $report | to json | save -f $"build-report-($host).json"

    # Non-zero exit marks the matrix leg as failed in the UI. The artifact
    # upload step uses `if: always()` so the report is still preserved.
    if $report.status == "failed" {
        exit 1
    }
}

# Seed this container's local /nix/store from the host nix daemon, using the
# previous successful build's toplevel paths recorded in .ci/last-builds.json.
# Best-effort: paths that have been garbage-collected on the daemon are
# silently skipped (the build falls back to cache.nixos.org as usual).
def seed_local_store [host: string] {
    let state_file = ".ci/last-builds.json"
    if not ($state_file | path exists) {
        print $"=== No ($state_file); skipping seed for ($host) ==="
        return
    }

    let last = open $state_file
    let seed_paths = ($last | get -i $host | default [])
    if ($seed_paths | is-empty) {
        print $"=== No seed paths recorded for ($host); skipping seed ==="
        return
    }

    print $"=== Seeding /nix/store from daemon for ($host) \(($seed_paths | length) path\(s\)\) ==="

    # nix copy with multiple paths is atomic — if any listed path is missing
    # on the source, the whole operation errors. Iterate per-path so a
    # garbage-collected entry doesn't abort the seed. --no-check-sigs because
    # paths built on the daemon are unsigned.
    for p in $seed_paths {
        let r = ^nix copy --from daemon --no-check-sigs $p | complete
        if $r.exit_code == 0 {
            print $"  ✓ seeded ($p)"
        } else {
            let msg = ($r.stderr | lines | last 2 | str join " " | str trim)
            print $"  ! skipped ($p): ($msg)"
        }
    }
}
