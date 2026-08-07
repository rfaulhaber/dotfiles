#!/usr/bin/env nu

# Emit the build job's strategy.matrix as a GITHUB_OUTPUT line, sourced from
# .github/hosts.json. The eval workflow reads the same file, so which hosts
# CI covers is a one-line JSON change in exactly one place.

let hosts = open .github/hosts.json
let matrix = { include: $hosts }
$"matrix=($matrix | to json -r)\n" | save --append $env.GITHUB_OUTPUT
