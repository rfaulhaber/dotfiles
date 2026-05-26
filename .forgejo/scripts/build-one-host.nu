#!/usr/bin/env nu

# Build a single NixOS configuration, copy its closure to the host nix daemon,
# and write the per-host build report to $CI_RUN_DIR. Intended to run in a
# strategy.matrix leg — one invocation per host.
#
# Before building, seeds the container's local /nix/store with the previous
# successful build's closure for this host, using the path list persisted at
# /ci-state/seed/<host>/last-paths.json. /ci-state is a host-side bind mount
# configured on the forgejo-runner (modules.linux.oci.services.forgejo-runner.
# runners.<name>.jobStateDir) — each matrix leg writes its own file, so there
# is no cross-host contention and no committed git state.
#
# Two namespaces under /ci-state:
#   /ci-state/seed/<host>/ — persistent per-host seed state (survives runs)
#   /ci-state/runs/<run>/  — per-run scratch (build reports, input-changes)
#
# NOTE: We intentionally do NOT set NIX_REMOTE=daemon. Mixing a container-local
# /nix/store with the host daemon's /nix/store breaks flake input fetching: the
# client-side evaluator reads store paths via open(), but with NIX_REMOTE=daemon
# the fetched paths only exist on the host's filesystem — not inside the
# container — and evaluation fails with "opening file '...-source': No such
# file or directory".

const seed_root = "/ci-state/seed"

def main [host: string] {
    let run_dir = $env.CI_RUN_DIR
    mkdir $run_dir

    seed_local_store $host

    let attr = $".#nixosConfigurations.($host).config.system.build.toplevel"

    print $"=== Building ($host) ==="
    let start = date now
    # Stream nix's stderr live to the terminal so CI logs show build progress
    # in real time, while also saving a copy to $stderr_file for the error
    # tail in the build report. `err>|` pipes nix's stderr into tee's stdin;
    # tee writes one copy to disk via the closure and passes the stream
    # through, which `print` forwards to the terminal. `do --ignore-errors`
    # swallows a non-zero nix exit; we determine success below by checking
    # whether the toplevel materialized in /nix/store, because LAST_EXIT_CODE
    # is unreliable here (do --ignore-errors clears it to 0 unconditionally,
    # and the tee pipeline would mask it too).
    #
    # We intentionally do NOT use `--print-out-paths` + stdout capture here:
    # combining stdout capture with a tee'd stderr pipeline is fragile across
    # nushell versions. Instead we resolve the toplevel store path via
    # `nix eval --raw …outPath`, which is deterministic and ~free (the
    # attribute is already in nix's eval cache from the build we just ran).
    let stderr_file = (^mktemp --suffix .log | str trim)
    # Parens wrap the pipeline so nushell attaches `err>| tee …` to the
    # `^nix build` invocation across line breaks; without them the parser
    # raises "Unexpected redirection." on the continuation line.
    do --ignore-errors {
        (
            ^nix build $attr --no-link --print-build-logs
            err>| tee { save --force --raw $stderr_file }
            | print
        )
    }
    let stderr_content = if ($stderr_file | path exists) {
        open --raw $stderr_file | decode utf-8
    } else { "" }
    rm --force $stderr_file

    let elapsed = (date now) - $start | format duration min

    # Determine success by checking whether the toplevel actually materialized
    # in /nix/store, not by `$env.LAST_EXIT_CODE`. `do --ignore-errors` clears
    # LAST_EXIT_CODE to 0 regardless of what nix exited with, and piping nix's
    # stderr through `tee` (kept for live CI log streaming) would mask it too.
    # The store path is the authoritative signal: if it's present the closure
    # is ready to copy, if not the build failed regardless of what nix reported.
    let eval_result = (^nix eval --raw $"($attr).outPath" | complete)
    let out_path = ($eval_result.stdout | str trim)

    let report = if $eval_result.exit_code == 0 and (not ($out_path | is-empty)) and ($out_path | path exists) {
        let out_paths = [$out_path]
        print $"  ✓ ($host) built successfully \(($elapsed)\)"

        # Copy the built closure to the host daemon so other machines can
        # pull it via harmonia. --no-check-sigs is required because we
        # build unsigned inside the container; the daemon socket's root
        # user is a trusted-user on vulcan.
        print $"=== Copying ($host) closure to host nix daemon ==="
        let copy_result = ^nix copy --to daemon --no-check-sigs ...$out_paths | complete
        if $copy_result.exit_code == 0 {
            print $"  ✓ ($host) copied to host store"
            # Refresh this host's seed state for the next run. Only
            # written after a full success so a transient copy failure
            # doesn't erase the warm cache.
            persist_seed $host $out_paths
            { host: $host, status: "success", elapsed: $elapsed, error: "", paths: $out_paths }
        } else {
            let err_tail = ($copy_result.stderr | lines | last 10 | str join "\n")
            print $"  ✗ ($host) copy failed"
            print $"    ($err_tail)"
            { host: $host, status: "failed", elapsed: $elapsed, error: $"copy to daemon failed: ($err_tail)", paths: $out_paths }
        }
    } else {
        let err_tail = ($stderr_content | lines | last 20 | str join "\n")
        print $"  ✗ ($host) failed \(($elapsed)\)"
        print $"    ($err_tail)"
        { host: $host, status: "failed", elapsed: $elapsed, error: $err_tail, paths: [] }
    }

    $report | to json | save -f $"($run_dir)/build-report-($host).json"

    # Non-zero exit marks the matrix leg as failed in the UI. The report file
    # was already written, so finalize can still surface this host's status.
    if $report.status == "failed" {
        exit 1
    }
}

# Seed this container's local /nix/store from the host nix daemon, using the
# previous successful build's toplevel paths recorded under /ci-state/seed.
# Best-effort: paths that have been garbage-collected on the daemon are
# silently skipped (the build falls back to cache.nixos.org as usual).
def seed_local_store [host: string] {
    let state_file = $"($seed_root)/($host)/last-paths.json"
    if not ($state_file | path exists) {
        print $"=== No ($state_file); skipping seed for ($host) ==="
        return
    }

    let seed_paths = open $state_file
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

# Persist this host's new toplevel paths as the seed for the next run.
def persist_seed [host: string, paths: list<string>] {
    let host_dir = $"($seed_root)/($host)"
    let state_file = $"($host_dir)/last-paths.json"
    mkdir $host_dir
    $paths | to json | save -f $state_file
    print $"=== Persisted ($paths | length) seed path\(s\) to ($state_file) ==="
}
