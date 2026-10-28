#!/usr/bin/env nu

# Front end for the local llama.cpp router, which runs as the user's own systemd
# unit, so nothing here needs sudo. The router loads a model on the first
# request that names it, so `status` and the readiness probe stick to routes
# that never do. The endpoint comes from $env.LLM_SERVER_URL.

const unit = "llama-server.service"

def endpoint []: nothing -> string {
    $env.LLM_SERVER_URL? | default "http://127.0.0.1:8080"
}

# `is-active` exits non-zero for every state but "active", so it is captured
# rather than allowed to fail the caller.
def unit-state []: nothing -> string {
    ^systemctl --user is-active $unit | complete | get stdout | str trim
}

# Every model the router knows about, with its load state. Empty when the router
# is not reachable.
def models []: nothing -> table {
    try {
        http get --max-time 5sec $"(endpoint)/v1/models"
            | get data
            | each {|model| {id: $model.id, status: $model.status?.value?} }
    } catch { [] }
}

# GPU memory as "<used> / <total> MiB". Null when nvidia-smi is missing or
# fails, which it does after a driver/library mismatch and has done by crashing
# on this host, so status never depends on it.
def vram []: nothing -> oneof<string, nothing> {
    try {
        let result = ^nvidia-smi --query-gpu=memory.used,memory.total --format=csv,noheader,nounits | complete
        if $result.exit_code != 0 { return null }
        let fields = $result.stdout | lines | first | split row "," | str trim
        $"($fields.0) / ($fields.1) MiB"
    } catch { null }
}

# The router answers /health as soon as it is listening, before any model loads.
def ready []: nothing -> bool {
    try {
        (http get --full --allow-errors --max-time 2sec $"(endpoint)/health").status == 200
    } catch { false }
}

def main []: nothing -> record {
    main status
}

# Show the unit state, every model's load state and GPU memory use.
def "main status" []: nothing -> record {
    {unit: (unit-state), models: (models), vram: (vram)}
}

# Start the router and wait until it answers /health.
def "main up" [] {
    let started = ^systemctl --user start $unit | complete
    if $started.exit_code != 0 {
        error make {msg: $"failed to start ($unit)", help: ($started.stderr | str trim)}
    }

    # The unit is checked before the endpoint so that a router that died on
    # startup (a bad preset, a taken port) is reported instead of being taken
    # for ready on the strength of whatever else answers there.
    let deadline = (date now) + 30sec
    while (date now) < $deadline {
        sleep 500ms
        if (unit-state) in ["failed" "inactive"] { break }
        if (ready) { return }
    }

    ^journalctl --user -u $unit -n 20 --no-pager | print --stderr
    error make {msg: "llama-server did not become ready"}
}

# Stop the router. Returns once it and every model process have exited.
def "main down" [] {
    ^systemctl --user stop $unit
}

# Flip the router between running and stopped.
def "main toggle" [] {
    if (unit-state) == "active" {
        main down
    } else {
        main up
    }
}
