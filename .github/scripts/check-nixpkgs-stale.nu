#!/usr/bin/env nu

# Compare the nixpkgs rev pinned in flake.lock against the latest commit
# on nixos/nixpkgs nixos-unstable. Sets the "stale" output for the workflow.

let lock = open flake.lock | from json

# Walk the lock to find the nixpkgs node
let root_nixpkgs = $lock.nodes.root.inputs.nixpkgs
let pinned_rev = $lock.nodes | get $root_nixpkgs | get locked.rev

print $"Pinned nixpkgs rev: ($pinned_rev)"

# Anonymous requests to api.github.com share a 60/hr rate limit across every
# CI tenant hitting it from the same IP, which flakes this gate on a busy
# runner. GH_API_TOKEN (wired from the workflow's built-in GITHUB_TOKEN, so it
# costs no repo configuration) raises that to the token's own far larger
# quota; unset is fine, the call just goes anonymous.
let auth_headers = if ("GH_API_TOKEN" in $env) and (not ($env.GH_API_TOKEN | is-empty)) {
    { Authorization: $"Bearer ($env.GH_API_TOKEN)" }
} else {
    {}
}

# --allow-errors so a non-2xx comes back as data (with a status code to
# classify) instead of an uncatchable error; --full for the status/headers
# needed to tell a rate limit apart from any other API failure.
let response = (http get --full --allow-errors --headers $auth_headers
  "https://api.github.com/repos/NixOS/nixpkgs/commits/nixos-unstable")

if $response.status != 200 {
    let remaining = ($response.headers.response
      | where name == "x-ratelimit-remaining"
      | get --optional 0.value)
    # A rate limit reads as 403/429 with the remaining-quota header at 0;
    # everything else in the 4xx/5xx range is a genuine API problem. Either
    # way we can't tell whether nixpkgs actually moved, so this run aborts
    # the same as an uncaught error always has — no PR beats one built
    # against a staleness check that never happened.
    if ($response.status in [403 429]) and ($remaining == "0") {
        print --stderr $"ERROR: GitHub API rate limit hit checking nixpkgs staleness \(status ($response.status)\)."
    } else {
        print --stderr $"ERROR: GitHub API request failed checking nixpkgs staleness \(status ($response.status)\)."
    }
    exit 1
}

let upstream_rev = $response.body.sha

print $"Upstream nixos-unstable rev: ($upstream_rev)"

let is_stale = $pinned_rev != $upstream_rev

print $"Stale: ($is_stale)"

# Set workflow output
let output_file = $env.GITHUB_OUTPUT
$"stale=($is_stale)\n" | save --append $output_file
