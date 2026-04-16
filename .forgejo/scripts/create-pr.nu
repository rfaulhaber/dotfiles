#!/usr/bin/env nu

# Create a Forgejo PR from the already-pushed flake-update branch, using the
# merged build-report.json produced by merge-reports.nu. The prepare job is
# responsible for committing and pushing the branch; this script only calls
# the pulls API.

def main [branch_name: string, date_str: string] {
    let report = open build-report.json

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
}
