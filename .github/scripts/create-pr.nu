#!/usr/bin/env nu

# Open — or refresh — the PR for the already-pushed flake-update branch, using
# the merged build-report.json produced by merge-reports.nu. The prepare job is
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
        let elapsed = ($b.elapsed | into duration --unit sec)
        # Four-backtick fence: the body is a raw nix build tail, and a log line
        # containing a triple backtick would otherwise close the fence early
        # and mangle every row after it.
        let detail = if $b.status == "failed" {
            $"\n<details><summary>Error</summary>\n\n````\n($b.error)\n````\n</details>"
        } else { "" }
        $"| ($icon) ($b.host) | ($b.status) | ($elapsed) |($detail)"
    } | str join "\n")

    let builds_section = $"| Host | Status | Time |\n| --- | --- | --- |\n($build_rows)"

    let pr_body = $"## Updated Flake Inputs

($inputs_section)

## Build Results

($builds_section)
"

    let api_base = $env.GITHUB_API_URL
    let repo = $env.GITHUB_REPOSITORY
    let owner = $env.GITHUB_REPOSITORY_OWNER
    let headers = {
        Authorization: $"Bearer ($env.GITHUB_TOKEN)"
        Accept: "application/vnd.github+json"
        "X-GitHub-Api-Version": "2022-11-28"
    }

    # Every call uses --allow-errors --full so a non-2xx arrives as data to
    # classify instead of aborting the job mid-flight, which would leave the
    # pushed branch with no PR pointing at it.
    #
    # The branch is date-stamped, so a second run on the same day (the daily
    # schedule plus a manual dispatch) reuses one that already has an open PR;
    # POSTing again is a 422. Update that PR instead so its tables reflect the
    # newest build.
    let existing = (http get --full --allow-errors --headers $headers
        $"($api_base)/repos/($repo)/pulls?head=($owner):($branch_name)&state=open")

    if $existing.status != 200 {
        print --stderr $"ERROR: could not list open PRs for ($branch_name) \(status ($existing.status)\): ($existing.body | to json -r)"
        exit 1
    }

    if ($existing.body | is-not-empty) {
        let number = ($existing.body | first | get number)
        let response = (http patch
            --full
            --allow-errors
            --content-type "application/json"
            --headers $headers
            $"($api_base)/repos/($repo)/pulls/($number)"
            { title: $"Flake bump ($date_str)", body: $pr_body })

        if $response.status != 200 {
            print --stderr $"ERROR: failed to update PR #($number) \(status ($response.status)\): ($response.body | to json -r)"
            exit 1
        }

        print $"PR updated: ($response.body.html_url)"
    } else {
        let response = (http post
            --full
            --allow-errors
            --content-type "application/json"
            --headers $headers
            $"($api_base)/repos/($repo)/pulls"
            {
                title: $"Flake bump ($date_str)"
                body: $pr_body
                head: $branch_name
                base: "main"
            })

        if $response.status != 201 {
            print --stderr $"ERROR: failed to create PR for ($branch_name) \(status ($response.status)\): ($response.body | to json -r)"
            exit 1
        }

        print $"PR created: ($response.body.html_url)"
    }
}
