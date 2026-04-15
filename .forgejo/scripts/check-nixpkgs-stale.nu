#!/usr/bin/env nu

# Compare the nixpkgs rev pinned in flake.lock against the latest commit
# on nixos/nixpkgs nixos-unstable. Sets the "stale" output for the workflow.

let lock = open flake.lock | from json

# Walk the lock to find the nixpkgs node
let root_nixpkgs = $lock.nodes.root.inputs.nixpkgs
let pinned_rev = $lock.nodes | get $root_nixpkgs | get locked.rev

print $"Pinned nixpkgs rev: ($pinned_rev)"

# Query GitHub API for the latest commit on nixos-unstable
let upstream = (http get "https://api.github.com/repos/NixOS/nixpkgs/commits/nixos-unstable"
  | select sha)
let upstream_rev = $upstream.sha

print $"Upstream nixos-unstable rev: ($upstream_rev)"

let is_stale = $pinned_rev != $upstream_rev

print $"Stale: ($is_stale)"

# Set workflow output
let output_file = $env.GITHUB_OUTPUT
$"stale=($is_stale)\n" | save --append $output_file
