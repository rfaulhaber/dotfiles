#!/usr/bin/env nu

# Emit the build job's strategy.matrix as a GITHUB_OUTPUT line, sourced from
# .forgejo/hosts.json. merge-reports.nu checks completed build reports
# against that same file, so the matrix and the completeness check can never
# drift apart — editing the host list is a one-line JSON change.

let hosts = open .forgejo/hosts.json
let matrix = { include: $hosts }
$"matrix=($matrix | to json -r)\n" | save --append $env.GITHUB_OUTPUT
