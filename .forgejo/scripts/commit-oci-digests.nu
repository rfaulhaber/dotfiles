#!/usr/bin/env nu

# Commit the JSON edits left behind by update-oci-digests.nu to a fresh
# update branch and push it. Split out so the digest-refresh script can
# run locally without making branches or pushing.
#
# Args:
#   date_str — UTC date stamp (YYYYMMDD) emitted by update-oci-digests.nu
#              as its `date` output; used for the branch name and commit
#              message so they line up with the report and downstream PR.
#
# Outputs (via $env.GITHUB_OUTPUT):
#   branch — name of the pushed update branch

def main [date_str: string] {
  let branch_name = $"oci-update-($date_str)"

  print $"=== Committing OCI digest updates to ($branch_name) ==="
  git config user.name "forgejo-actions[bot]"
  git config user.email "forgejo-actions[bot]@noreply.localhost"
  git checkout -b $branch_name
  git add nix/hosts/*/oci-images.json
  git commit -m $"oci: refresh image digests ($date_str)"
  git push -u origin $branch_name --force-with-lease

  let output_file = ($env.GITHUB_OUTPUT? | default "/dev/null")
  $"branch=($branch_name)\n" | save --append $output_file
}
