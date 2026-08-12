#!/usr/bin/env nu

# Run `nix flake update nixpkgs`, compute a diff of root-level inputs before
# and after, commit the updated flake.lock to a new branch, and push it. The
# diff is written to input-changes.json in the workspace root; the workflow
# uploads it as an artifact for the finalize job to render into the PR body.
#
# Outputs (via $env.GITHUB_OUTPUT):
#   branch — the name of the update branch (e.g. flake-update-20260416)
#   date   — the UTC date string used in the branch name and PR title

# --- Capture before state ---
print "=== Capturing pre-update flake metadata ==="
let before = (nix flake metadata --json | from json)

# --- Update nixpkgs ---
print "=== Updating nixpkgs ==="
nix flake update nixpkgs

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
                $before_node.locked.lastModified | into datetime --format "%s" | format date "%Y-%m-%d"
            } else { "n/a" }
            let after_date = if $after_rev != "n/a" {
                $after_node.locked.lastModified | into datetime --format "%s" | format date "%Y-%m-%d"
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

$input_changes | to json | save -f input-changes.json

print "Updated inputs:"
if ($input_changes | is-empty) {
    print "  (none changed)"
} else {
    $input_changes | table | print
}

# --- Commit and push update branch ---
let date_str = date now | date to-timezone UTC | format date "%Y%m%d"
let branch_name = $"flake-update-($date_str)"

print $"=== Committing flake.lock to ($branch_name) ==="
git config user.name "github-actions[bot]"
git config user.email "41898282+github-actions[bot]@users.noreply.github.com"
git checkout -b $branch_name
git add flake.lock
git commit -m $"flake: bump inputs ($date_str)"

# The checkout clones a single ref, so nothing maps refs/heads/$branch_name to
# a remote-tracking ref and a bare --force-with-lease is rejected as "stale
# info" whenever the branch already exists — which it does on any second run
# of the same day (the schedule plus a manual dispatch). Naming the expected
# value keeps the lease honest: an empty string asserts the branch does not
# exist yet, and the named object need not be present in this shallow clone.
let remote_head = (^git ls-remote origin $"refs/heads/($branch_name)"
    | split row "\t"
    | first
    | str trim)
git push -u origin $branch_name $"--force-with-lease=refs/heads/($branch_name):($remote_head)"

# --- Expose branch name and date as step outputs ---
let output_file = $env.GITHUB_OUTPUT
$"branch=($branch_name)\ndate=($date_str)\n" | save --append $output_file
