#!/usr/bin/env nu

# Compose NIX_CONFIG for the current job, dropping the substituter URL
# that points at the host this runner sits on. Self-references hairpin
# through the podman bridge and time out (the cache's firewall rule is
# scoped to the LAN interface, not container bridges) — widening that
# firewall would expose harmonia to every container on the host.
#
# Bootstrap note: the workflow does NOT set NIX_CONFIG at the env level,
# so the bootstrap `nix run nixpkgs#nushell -- configure-nix.nu` falls
# through to the host nix-daemon's own substituters (which never include
# the host's own cache). That avoids the self-reference timeout long
# enough to set the richer per-job NIX_CONFIG written here. The bootstrap
# invocation must pass --extra-experimental-features 'nix-command flakes'
# itself: the nixos/nix image ships no experimental-features in its
# nix.conf, and the gate is checked by the CLI, not the daemon.
#
# CI_RUNNER_HOST is injected into every job container by forgejo-runner.nix
# (set to the host the runner sits on). We deliberately do NOT fall back to
# RUNNER_NAME: not every Forgejo instance populates it (Codeberg's does
# not), and an empty value used to leave the self-referencing cache in the
# list — nix then retried it 15 times with exponential backoff, hanging the
# job for hours. If the host can't be identified, drop BOTH host caches: a
# missing cache only costs a slower build, a self-reference costs a hang.

if ($env.GITHUB_ENV? | is-empty) {
  print -e "configure-nix.nu: GITHUB_ENV not set; refusing to run outside CI."
  exit 1
}

let local_host = ($env.CI_RUNNER_HOST? | default "")
if ($local_host | is-empty) {
  print -e "configure-nix.nu: CI_RUNNER_HOST unset; excluding all host caches to avoid a self-reference hang."
}

let base_substituters = [
  "https://install.determinate.systems"
  "https://nix-community.cachix.org"
  "https://nixos-raspberrypi.cachix.org"
  "https://niri.cachix.org"
]

let host_substituters = (
  ["vulcan" "prometheus"]
  | where {|h| ($local_host | is-not-empty) and ($h != $local_host)}
  | each {|h| $"http://($h).lan:4965"}
)

let substituters = ($base_substituters ++ $host_substituters) | str join " "

let trusted_keys = [
  "cache.flakehub.com-3:hJuILl5sVK4iKm86JzgdXW12Y2Hwd5G07qKtHTOcDCM="
  "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  "nixos-raspberrypi.cachix.org-1:4iMO9LXa8BqhU+Rpg6LQKiGa2lsNh/j2oiYLNOQ5sPI="
  "niri.cachix.org-1:Wv0OmO7PsuocRKzfDoJ3mulSl7Z6oezYhGhR+3W2964="
  "vulcan.lan-1:Zu8N+6EtaIeDTyCVpR15uvIYYByZqMmd8W09vu8GKl8="
  "prometheus.lan-1:GetZTCVHg6NcVVteshbEZQbyMzZfIATcsIgt7si5Lmo="
] | str join " "

# cache.nixos.org over HTTP/2 occasionally drops large NARs mid-download
# with framing errors; default of 5 attempts isn't always enough to resume
# through to the end. Retries resume from the last byte via range requests.
let nix_config = $"experimental-features = nix-command flakes ca-derivations pipe-operators
extra-substituters = ($substituters)
extra-trusted-public-keys = ($trusted_keys)
download-attempts = 15
"

$"NIX_CONFIG<<__NIX_CONFIG_EOF__\n($nix_config)__NIX_CONFIG_EOF__\n"
| save --append $env.GITHUB_ENV

print $"Configured NIX_CONFIG for host '($local_host)'; substituters: ($substituters)"
