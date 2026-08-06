#!/usr/bin/env nu

# Create a Forgejo PR from the already-pushed oci-update branch, using the
# oci-changes.json report produced by update-oci-digests.nu. The refresh job
# is responsible for committing and pushing the branch; this script only
# calls the pulls API.

def main [branch_name: string, date_str: string] {
  let report_path = ($env.CI_RUN_DIR | path join "oci-changes.json")
  let changes = (open $report_path)

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
  let failures_path = ($env.CI_RUN_DIR | path join "oci-validation-failures.json")
  let failures = if ($failures_path | path exists) { open $failures_path } else { [] }
  let warning = if ($failures | is-empty) {
    ""
  } else {
    let failed_hosts = ($failures | get host | str join ", ")
    $"\n> [!WARNING]\n> Toplevel eval failed for ($failed_hosts) after this digest bump. This may be a pre-existing break unrelated to the change above — verify with `nix eval` before merging.\n"
  }

  let pr_body = $"## OCI image digest refresh

Refreshed ($changes | length) image\(s\) across ($by_host | columns | length) host\(s\).
($warning)
($sections)

Each pinned tag was re-resolved against its registry; only digests where the
upstream manifest moved appear above. The full digest values land in the
host's `oci-images.json` — see the diff for the new SHAs.
"

  let api_base = $env.GITHUB_SERVER_URL
  let repo = $env.GITHUB_REPOSITORY
  let token = $env.GITHUB_TOKEN

  let pr_payload = {
    title: $"OCI image digests ($date_str)"
    body: $pr_body
    head: $branch_name
    base: "main"
  }

  let response = (http post
    --content-type "application/json"
    --headers {Authorization: $"token ($token)"}
    $"($api_base)/api/v1/repos/($repo)/pulls"
    $pr_payload)

  print $"PR created: ($response.html_url)"
}
