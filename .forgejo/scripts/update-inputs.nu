#!/usr/bin/env nu

# Run `nix flake update`, compute a diff of root-level inputs before and after,
# commit the updated flake.lock to a new branch, and push it. The diff is
# written to $CI_RUN_DIR/input-changes.json for the finalize job to pick up
# via the shared /ci-state bind mount (no artifact actions needed).
#
# Outputs (via $env.GITHUB_OUTPUT):
#   branch — the name of the update branch (e.g. flake-update-20260416)
#   date   — the UTC date string used in the branch name and PR title

let run_dir = $env.CI_RUN_DIR
mkdir $run_dir

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

$input_changes | to json | save -f $"($run_dir)/input-changes.json"

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
git config user.name "forgejo-actions[bot]"
git config user.email "forgejo-actions[bot]@noreply.localhost"
git checkout -b $branch_name
git add flake.lock
git commit -m $"flake: bump inputs ($date_str)"
git push -u origin $branch_name

# --- Expose branch name and date as step outputs ---
let output_file = $env.GITHUB_OUTPUT
$"branch=($branch_name)\ndate=($date_str)\n" | save --append $output_file
