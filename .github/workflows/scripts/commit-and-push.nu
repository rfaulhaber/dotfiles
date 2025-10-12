let date_str = date now
    | date to-timezone UTC
    | format date $env.BRANCH_DATE_FORMAT
let branch_name = $"update-nix-flake-($date_str)"

git config user.name "github-actions[bot]"
git config user.email "github-actions[bot]@users.noreply.github.com"

git checkout -b $branch_name
git add flake.lock
git commit -m "Flake bump"
git push -u origin $branch_name

let pr_body = open output

gh pr create --title $"Flake bump ($date_str)" --body $pr_body --base $env.DEFAULT_BRANCH --head $branch_name
