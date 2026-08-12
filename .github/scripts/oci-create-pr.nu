#!/usr/bin/env nu

# Open (or refresh) the pull request for an already-pushed oci-update branch,
# using the oci-changes.json report produced by bin/update-oci-digests.nu. The
# refresh job is responsible for committing and pushing the branch; this
# script only calls the pulls API.
#
# Env:
#   GITHUB_TOKEN — must be passed explicitly in the step's `env:`; unlike
#                  Forgejo, GitHub does not inject it into `run:` steps.
#   OCI_REPORT_DIR — where update-oci-digests.nu wrote its reports.
#   GITHUB_API_URL, GITHUB_REPOSITORY — runner-injected; read without a
#                  fallback, so running this outside Actions fails on the
#                  missing column rather than posting somewhere unintended.

def main [branch_name: string, date_str: string] {
  let report_dir = ($env.OCI_REPORT_DIR? | default "/tmp")
  let changes = (open ($report_dir | path join "oci-changes.json"))

  # Group changes by host so the PR body is scannable per-deployment.
  let by_host = ($changes | group-by host)

  let sections = ($by_host | columns | each { |h|
    let entries = ($by_host | get $h)
    let rows = ($entries | each { |e|
      let path_str = ($e.path | str join ".")
      let old_short = ($e.old_digest | default "(unset)" | str substring 0..19)
      let new_short = ($e.new_digest | str substring 0..19)
      $"| `($path_str)` | `($e.repo):($e.version)` | `($old_short)…` | `($new_short)…` |"
    } | str join "\n")
    $"### ($h)\n\n| Module path | Image | Old | New |\n| --- | --- | --- | --- |\n($rows)"
  } | str join "\n\n")

  # update-oci-digests.nu validates each affected host's toplevel but doesn't
  # abort the run on failure — a host broken for unrelated reasons shouldn't
  # discard every other host's digest bump. Surface those failures here so a
  # reviewer knows which hosts still need a manual eval check before merging.
  let failures_path = ($report_dir | path join "oci-validation-failures.json")
  let failures = if ($failures_path | path exists) { open $failures_path } else { [] }
  let warning = if ($failures | is-empty) {
    ""
  } else {
    let failed_hosts = ($failures | get host | str join ", ")
    $"\n> [!WARNING]\n> Toplevel eval failed for ($failed_hosts) after this digest bump. This may be a pre-existing break unrelated to the change above — verify with `nix eval` before merging.\n"
  }

  # Entries whose repo or tag would not resolve are skipped rather than
  # aborting the refresh, so they produce no diff at all. Without this block
  # a permanently dead tag is invisible: it just silently stops being
  # updated, which is how recyclarr sat on a retired :latest for months.
  let fetch_path = ($report_dir | path join "oci-fetch-failures.json")
  let fetch_failures = if ($fetch_path | path exists) { open $fetch_path } else { [] }
  let fetch_warning = if ($fetch_failures | is-empty) {
    ""
  } else {
    let rows = ($fetch_failures | each { |f|
      let path_str = ($f.path | str join ".")
      $"> | `($f.host)` | `($path_str)` | `($f.repo | default "?"):($f.version)` |"
    } | str join "\n")
    $"\n> [!CAUTION]\n> ($fetch_failures | length) image\(s\) could not be resolved and were **not** refreshed. A tag that stopped resolving usually means upstream retired it — check the image's current tags before assuming the pin is still valid.\n>\n> | Host | Module path | Image |\n> | --- | --- | --- |\n($rows)\n"
  }

  # The same blocks go to the job summary: a warning buried in a PR body is
  # easy to scroll past, and a retired tag produces no diff to notice.
  if (not ($warning | is-empty)) or (not ($fetch_warning | is-empty)) {
    let summary_file = ($env.GITHUB_STEP_SUMMARY? | default "/dev/null")
    $"## OCI digest refresh ($date_str)\n($warning)($fetch_warning)\n" | save --append $summary_file
  }

  let pr_body = $"## OCI image digest refresh

Refreshed ($changes | length) image\(s\) across ($by_host | columns | length) host\(s\).
($warning)($fetch_warning)
($sections)

Each pinned tag was re-resolved against its registry; only digests where the
upstream manifest moved appear above. The full digest values land in the
host's `oci-images.json` — see the diff for the new SHAs.
"

  let api_base = $env.GITHUB_API_URL
  let repo = $env.GITHUB_REPOSITORY
  let owner = ($repo | split row "/" | first)
  let headers = {
    Authorization: $"Bearer ($env.GITHUB_TOKEN)"
    Accept: "application/vnd.github+json"
    "X-GitHub-Api-Version": "2022-11-28"
  }

  # The branch name is only date-stamped, so a same-day re-dispatch force-pushes
  # over a branch that already has an open PR. POSTing again there fails with a
  # 422 that reads like a permissions problem; update the existing PR instead so
  # the body always matches the branch's current contents.
  let listed = (http get --allow-errors --full --headers $headers
    $"($api_base)/repos/($repo)/pulls?state=open&head=($owner):($branch_name)")
  if $listed.status != 200 {
    error make {msg: $"listing open PRs for ($branch_name) failed: HTTP ($listed.status)"}
  }
  let existing = ($listed.body | where head.ref == $branch_name)

  let pr = if ($existing | is-empty) {
    let created = (http post --allow-errors --full
      --content-type "application/json"
      --headers $headers
      $"($api_base)/repos/($repo)/pulls"
      {
        title: $"OCI image digests ($date_str)"
        body: $pr_body
        head: $branch_name
        base: "main"
      })
    if $created.status != 201 {
      error make {msg: $"creating PR for ($branch_name) failed: HTTP ($created.status)"}
    }
    $created.body
  } else {
    let number = ($existing | first | get number)
    let updated = (http patch --allow-errors --full
      --content-type "application/json"
      --headers $headers
      $"($api_base)/repos/($repo)/pulls/($number)"
      {
        title: $"OCI image digests ($date_str)"
        body: $pr_body
      })
    if $updated.status != 200 {
      error make {msg: $"updating PR #($number) failed: HTTP ($updated.status)"}
    }
    $updated.body
  }

  print $"PR ready: ($pr.html_url)"
}
