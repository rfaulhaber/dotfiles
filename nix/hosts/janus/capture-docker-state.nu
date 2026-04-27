#!/usr/bin/env nu
# Capture pre-cutover Docker state on janus:
#   1. Container image digests — input for pinning images in configuration.nix
#   2. Tarballed contents of the named Docker volumes that don't survive the
#      Docker → Podman switch (netbird mgmt SQLite DB, signal state, LE certs)
#
# Run on janus as root, before `docker compose down`. See CUTOVER.org
# Phase 1 / Phase 2 for context.
#
#   sudo nu capture-docker-state.nu
#   sudo nu capture-docker-state.nu --output-dir /root/janus-volume-migration

def main [
  --output-dir: path = /root/janus-volume-migration
  --volumes: list<string> = [
    janus_netbird-mgmt
    janus_netbird-signal
    janus_netbird-letsencrypt
  ]
] {
  if (^id -u | into int) != 0 {
    print --stderr "Must be run as root (try: sudo nu capture-docker-state.nu)"
    exit 1
  }

  mkdir $output_dir

  print "==> Capturing container image digests"
  let digests = ^docker ps --format '{{.Names}}|{{.Image}}'
    | lines
    | each { |line|
        let parts = ($line | split row '|')
        {
          name: $parts.0,
          image: $parts.1,
          digest: (^docker inspect $parts.0 --format '{{index .Image}}'),
        }
      }

  let digest_file = ($output_dir | path join digests.json)
  $digests | to json | save --force $digest_file
  print $"    wrote ($digest_file)"
  print ($digests | table --expand)

  print ""
  print "==> Exporting Docker volumes"
  for vol in $volumes {
    let archive = $"($vol).tar.gz"
    print $"    ($vol) -> ($output_dir)/($archive)"
    (^docker run --rm
      -v $"($vol):/src:ro"
      -v $"($output_dir):/dst"
      alpine tar czf $"/dst/($archive)" -C /src .)
  }

  print ""
  print $"==> Done. Contents of ($output_dir):"
  ls $output_dir | select name size modified
}
