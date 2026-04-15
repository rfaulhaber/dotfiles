#!/usr/bin/env nu

# Commit the updated flake.lock, push a branch, and create a PR via Forgejo API
# with a report of what changed and which hosts built successfully.

let date_str = date now | date to-timezone UTC | format date "%Y%m%d"
let branch_name = $"flake-update-($date_str)"

# --- Read build report ---
let report = open build-report.json

# --- Format PR body ---

# Input changes table
let inputs_section = if ($report.input_changes | is-empty) {
    "No inputs changed."
} else {
    let rows = ($report.input_changes | each {|c|
        $"| ($c.input) | `($c.before_rev)` \(($c.before_date)\) | `($c.after_rev)` \(($c.after_date)\) |"
    } | str join "\n")
    $"| Input | Before | After |\n| --- | --- | --- |\n($rows)"
}

# Build results table
let build_rows = ($report.build_results | each {|b|
    let icon = if $b.status == "success" { "✅" } else { "❌" }
    let detail = if $b.status == "failed" {
        $"\n<details><summary>Error</summary>\n\n```\n($b.error)\n```\n</details>"
    } else { "" }
    $"| ($icon) ($b.host) | ($b.status) | ($b.elapsed) |($detail)"
} | str join "\n")

let builds_section = $"| Host | Status | Time |\n| --- | --- | --- |\n($build_rows)"

let pr_body = $"## Updated Flake Inputs

($inputs_section)

## Build Results

($builds_section)
"

# --- Git operations ---
git config user.name "forgejo-actions[bot]"
git config user.email "forgejo-actions[bot]@noreply.localhost"

git checkout -b $branch_name
git add flake.lock
git commit -m $"flake: bump inputs ($date_str)"
git push -u origin $branch_name

# --- Create PR via Forgejo API ---
let api_base = $env.GITHUB_SERVER_URL
let repo = $env.GITHUB_REPOSITORY
let token = $env.GITHUB_TOKEN

let pr_payload = {
    title: $"Flake bump ($date_str)"
    body: $pr_body
    head: $branch_name
    base: "main"
}

let response = (http post
    --content-type "application/json"
    --headers { Authorization: $"token ($token)" }
    $"($api_base)/api/v1/repos/($repo)/pulls"
    $pr_payload)

print $"PR created: ($response.html_url)"
